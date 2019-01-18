{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Data structure and JSON serializers for ipynb (Jupyter notebook) format.
The format is documented here:
<https://nbformat.readthedocs.io/en/latest/format_description.html>.
-}
module Data.Ipynb ( Notebook(..)
                  , NbV3
                  , NbV4
                  , JSONMeta
                  , Cell(..)
                  , Source(..)
                  , CellType(..)
                  , Output(..)
                  , MimeType
                  , MimeData(..)
                  , MimeBundle(..)
                  , breakLines
                  )
where
import Prelude
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Data.List (partition)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Control.Applicative ((<|>))
import qualified Data.ByteString.Base64 as Base64
import GHC.Generics
import Control.Monad (when)
import Data.Char (isSpace)

type MimeType = Text

data NbV3
data NbV4

data Notebook a = Notebook
  { n_metadata       :: JSONMeta
  , n_nbformat       :: (Int, Int)
  , n_cells          :: [Cell a]
  } deriving (Show, Eq, Generic)

instance Semigroup (Notebook a) where
  Notebook m1 f1 c1 <> Notebook m2 f2 c2 =
    Notebook (m1 <> m2) (max f1 f2) (c1 <> c2)

instance Monoid (Notebook a) where
  mempty = Notebook mempty (0, 0) mempty

instance FromJSON (Notebook NbV4) where
  parseJSON = withObject "Notebook" $ \v -> do
    fmt <- v .:? "nbformat" .!= 0
    when (fmt < 4 || fmt > 4) $ fail "expected nbformat == 4"
    fmtminor <- v .:? "nbformat_minor" .!= 0
    metadata <- v .:? "metadata" .!= mempty
    cells <- v .: "cells"
    return $
      Notebook{ n_metadata = metadata
              , n_nbformat = (fmt, fmtminor)
              , n_cells = cells
              }

instance FromJSON (Notebook NbV3) where
  parseJSON = withObject "Notebook" $ \v -> do
    fmt <- v .:? "nbformat" .!= 0
    when (fmt > 3) $ fail $ "expected nbformat <= 3"
    fmtminor <- v .:? "nbformat_minor" .!= 0
    metadata <- v .:? "metadata" .!= mempty
    worksheets <- v .: "worksheets"
    -- NOTE: we ignore metadata on worksheets: is this ever used?
    cells <- mconcat <$> mapM (.: "cells") worksheets
    return $
      Notebook{ n_metadata = metadata
              , n_nbformat = (fmt, fmtminor)
              , n_cells = cells
              }

instance ToJSON (Notebook NbV4) where
 toJSON n = object
   [ "nbformat" .= fst (n_nbformat n)
   , "nbformat_minor" .= snd (n_nbformat n)
   , "metadata" .= (n_metadata n)
   , "cells" .= (if n_nbformat n >= (4,1)
                    then id
                    else map (\c -> c{ c_attachments = Nothing }))
                (n_cells n)
   ]

instance ToJSON (Notebook NbV3) where
 toJSON n = object
   [ "nbformat" .= fst (n_nbformat n)
   , "nbformat_minor" .= snd (n_nbformat n)
   , "metadata" .= (n_metadata n)
   , "worksheets" .=
     [ object
       [ "cells" .= (if n_nbformat n >= (4,1)
                        then id
                        else map (\c -> c{ c_attachments = Nothing }))
                    (n_cells n)
       , "metadata" .= (mempty :: JSONMeta) -- see above in FromJSON instance
       ]
     ]
   ]

type JSONMeta = M.Map Text Value

newtype Source = Source{ unSource :: [Text] }
  deriving (Show, Eq, Generic, Semigroup, Monoid)

instance FromJSON Source where
  parseJSON v = do
    ts <- parseJSON v <|> (:[]) <$> parseJSON v
    return $ Source ts

instance ToJSON Source where
  toJSON (Source ts) = toJSON ts

data Cell a = Cell
  { c_cell_type        :: CellType a
  , c_source           :: Source
  , c_metadata         :: JSONMeta
  , c_attachments      :: Maybe (M.Map Text MimeBundle)
} deriving (Show, Eq, Generic)

instance FromJSON (Cell NbV4) where
  parseJSON = withObject "Cell" $ \v -> do
    ty <- v .: "cell_type"
    cell_type <-
      case ty of
        "markdown" -> pure Markdown
        "raw" -> pure Raw
        "code" ->
          Code
            <$> v .:? "execution_count"
            <*> v .: "outputs"
        _ -> fail $ "Unknown cell_type " ++ ty
    metadata <- v .: "metadata"
    attachments <- v .:? "attachments"
    source <- v .: "source"
    return
      Cell{ c_cell_type = cell_type
          , c_metadata = metadata
          , c_attachments = attachments
          , c_source = source
          }

instance FromJSON (Cell NbV3) where
  parseJSON = withObject "Cell" $ \v -> do
    ty <- v .: "cell_type"
    cell_type <-
      case ty of
        "markdown" -> pure Markdown
        "heading" -> Heading <$> v .: "level"
        "raw" -> pure Raw
        "code" ->
          Code
            <$> v .:? "prompt_number"
            <*> v .: "outputs"
        _ -> fail $ "Unknown cell_type " ++ ty
    metadata <- parseV3Metadata v
    attachments <- v .:? "attachments"
    source <- if ty == "code"
                 then v .: "input"
                 else v .: "source"
    return
      Cell{ c_cell_type = cell_type
          , c_metadata = metadata
          , c_attachments = attachments
          , c_source = source
          }

-- note that execution_count can't be omitted!
instance ToJSON (Cell NbV4) where
 toJSON c = object $
   ("metadata" .= c_metadata c) :
   maybe [] (\x -> ["attachments" .= x]) (c_attachments c) ++
   case c_cell_type c of
     Markdown -> [ "cell_type" .= ("markdown" :: Text)
                 , "source" .= c_source c ]
     Heading lev ->
                [ "cell_type" .= ("markdown" :: Text)
                , "source" .=
                     (Source . breakLines .
                      ((T.replicate lev "#" <> " ") <>) .
                      mconcat . unSource) (c_source c)
                 ]
     Raw      -> [ "cell_type" .= ("raw" :: Text)
                 , "source" .= c_source c
                 ]
     Code{
         c_execution_count = ec
       , c_outputs = outs
       }      -> [ "cell_type" .= ("code" :: Text)
                 , "execution_count" .= ec
                 , "outputs" .= outs
                 , "source" .= c_source c
                 ]

instance ToJSON (Cell NbV3) where
 toJSON c =
  object $
   metadataToV3Pairs (c_metadata c) ++
   case c_cell_type c of
     Markdown    -> [ "cell_type" .= ("markdown" :: Text)
                    , "source" .= c_source c
                    ]
     Heading lev -> [ "cell_type" .= ("heading" :: Text)
                    , "level" .= lev
                    , "source" .= c_source c
                    ]
     Raw         -> [ "cell_type" .= ("raw" :: Text)
                    , "source" .= c_source c
                    ]
     Code{
         c_execution_count = ec
       , c_outputs = outs
       }      -> [ "cell_type" .= ("code" :: Text)
                 , "input" .= c_source c
                 , "outputs" .= outs
                 ] ++
                 maybe [] (\n -> ["prompt_number" .= n]) ec

-- in v3, certain metadata fields occur in the main cell object.
-- e.g. collapsed, language.
metadataToV3Pairs :: JSONMeta -> [Aeson.Pair]
metadataToV3Pairs meta =
  ("metadata" .= M.fromList regMeta) : map toPair extraMeta
  where (extraMeta, regMeta) = partition isExtraMeta $ M.toList meta
        toPair (k,v) = k .= v

v3MetaInMainCell :: [Text]
v3MetaInMainCell = ["collapsed", "language"]

isExtraMeta :: (Text, a) -> Bool
isExtraMeta (k,_) = k `elem` v3MetaInMainCell

parseV3Metadata :: HM.HashMap Text Value -> Aeson.Parser JSONMeta
parseV3Metadata v = do
  meta <- v .: "metadata"
  let extraMeta = M.fromList $ filter isExtraMeta $ HM.toList v
  return (meta <> extraMeta)

data CellType a =
    Markdown
  | Heading -- V3 only
    { c_level  :: Int
    }
  | Raw
  | Code
    { c_execution_count  :: Maybe Int
    , c_outputs          :: [Output a]
    }
  deriving (Show, Eq, Generic)

data Output a =
    Stream
    { s_name            :: Text
    , s_text            :: Source }
  | Display_data
    { d_data            :: MimeBundle
    , d_metadata        :: JSONMeta
    }
  | Execute_result
    { e_execution_count :: Int
    , e_data            :: MimeBundle
    , e_metadata        :: JSONMeta
    }
  | Err
    { e_ename           :: Text
    , e_evalue          :: Text
    , e_traceback       :: [Text]
    }
  deriving (Show, Eq, Generic)

instance FromJSON (Output NbV4) where
  parseJSON = withObject "Object" $ \v -> do
    ty <- v .: "output_type"
    case ty of
      "stream" ->
        Stream
          <$> v .: "name"
          <*> v .: "text"
      "display_data" ->
        Display_data
          <$> v .: "data"
          <*> v .:? "metadata" .!= mempty
      "execute_result" ->
        Execute_result
          <$> v .: "execution_count"
          <*> v .: "data"
          <*> v .:? "metadata" .!= mempty
      "error" ->
        Err
          <$> v .: "ename"
          <*> v .: "evalue"
          <*> v .: "traceback"
      _ -> fail $ "Unknown object_type " ++ ty

instance FromJSON (Output NbV3) where
  parseJSON = withObject "Object" $ \v -> do
    ty <- v .: "output_type"
    case ty of
      "stream" ->
        Stream
          <$> v .: "stream"
          <*> v .: "text"
      "display_data" ->
        Display_data
          <$> extractNbV3Data v
          <*> v .:? "metadata" .!= mempty
      "pyout" ->
        Execute_result
          <$> v .: "prompt_number"
          <*> extractNbV3Data v
          <*> v .:? "metadata" .!= mempty
      "pyerr" ->
        Err
          <$> v .: "ename"
          <*> v .: "evalue"
          <*> v .: "traceback"
      _ -> fail $ "Unknown object_type " ++ ty

-- Remove keys output_type, prompt_number, metadata;
-- change short keys like text and png to mime types.
extractNbV3Data :: Aeson.Object -> Aeson.Parser MimeBundle
extractNbV3Data v = do
  let go ("output_type", _) = Nothing
      go ("metadata", _) = Nothing
      go ("prompt_number", _) = Nothing
      go ("text", x) = Just ("text/plain", x)
      go ("latex", x) = Just ("text/latex", x)
      go ("html", x) = Just ("text/html", x)
      go ("png", x)  = Just ("image/png", x)
      go ("jpeg", x)  = Just ("image/jpeg", x)
      go (_, _) = Nothing -- TODO complete list? where documented?
  parseJSON (Object . HM.fromList . mapMaybe go . HM.toList $ v)

instance ToJSON (Output NbV4) where
  toJSON s@(Stream{}) = object
    [ "output_type" .= ("stream" :: Text)
    , "name" .= s_name s
    , "text" .= s_text s
    ]
  toJSON d@(Display_data{}) = object
    [ "output_type" .= ("display_data" :: Text)
    , "data" .= d_data d
    , "metadata" .= d_metadata d
    ]
  toJSON e@(Execute_result{}) = object
    [ "output_type" .= ("execute_result" :: Text)
    , "execution_count" .= e_execution_count e
    , "data" .= e_data e
    , "metadata" .= e_metadata e
    ]
  toJSON e@(Err{}) = object
    [ "output_type" .= ("error" :: Text)
    , "ename" .= e_ename e
    , "evalue" .= e_evalue e
    , "traceback" .= e_traceback e
    ]

instance ToJSON (Output NbV3) where
  toJSON s@(Stream{}) = object
    [ "output_type" .= ("stream" :: Text)
    , "stream" .= s_name s
    , "text" .= s_text s
    ]
  toJSON d@(Display_data{}) =
    adjustV3DataFields $ object $
    [ "output_type" .= ("display_data" :: Text)
    , "data" .= d_data d
    ] ++ if M.null (d_metadata d)
            then []
            else ["metadata" .= d_metadata d]
  toJSON e@(Execute_result{}) =
    adjustV3DataFields $ object $
    [ "output_type" .= ("pyout" :: Text)
    , "prompt_number" .= e_execution_count e
    , "data" .= e_data e
    ] ++ if M.null (e_metadata e)
            then []
            else ["metadata" .= e_metadata e]
  toJSON e@(Err{}) = object $
    [ "output_type" .= ("pyerr" :: Text)
    , "ename" .= e_ename e
    , "evalue" .= e_evalue e
    , "traceback" .= e_traceback e
    ]

adjustV3DataFields :: Value -> Value
adjustV3DataFields (Object hm) =
  case HM.lookup "data" hm of
    Just (Object dm) -> Object $
      HM.delete "data" $ foldr
      (\(k, v) -> HM.insert (modKey k) v) hm
      (HM.toList dm)
    _ -> Object hm
  where  modKey "text/plain" = "text"
         modKey "text/latex" = "latex"
         modKey "text/html" = "html"
         modKey "image/jpeg" = "jpeg"
         modKey "image/png" = "png"
         modKey x = x
adjustV3DataFields x = x

data MimeData =
    BinaryData ByteString
  | TextualData Text
  | JsonData Value
  deriving (Show, Eq, Generic)

newtype MimeBundle = MimeBundle{ unMimeBundle :: M.Map MimeType MimeData }
  deriving (Show, Eq, Generic, Semigroup, Monoid)

instance FromJSON MimeBundle where
  parseJSON v = do
    m <- parseJSON v >>= mapM pairToMimeData . M.toList
    return $ MimeBundle $ M.fromList m

pairToMimeData :: (MimeType, Value) -> Aeson.Parser (MimeType, MimeData)
pairToMimeData ("application/json", v) =
  return $ ("application/json", JsonData v)
pairToMimeData (mt, v) = do
  t <- parseJSON v <|> (mconcat <$> parseJSON v)
  let mimeprefix = T.takeWhile (/='/') mt
  if mimeprefix == "text"
     then return (mt, TextualData t)
     else
       case Base64.decode (TE.encodeUtf8 (T.filter (not . isSpace) t)) of
            Left _ -> return (mt, TextualData t)
            Right b -> return (mt, BinaryData b)

instance ToJSON MimeBundle where
  toJSON (MimeBundle m) =
    let mimeBundleToValue (BinaryData bs) =
          toJSON $ TE.decodeUtf8 . Base64.joinWith "\n" 76 . Base64.encode $ bs
        mimeBundleToValue (JsonData v) = v
        mimeBundleToValue (TextualData t) = toJSON (breakLines t)
    in  toJSON $ M.map mimeBundleToValue m

breakLines :: Text -> [Text]
breakLines t =
  let (x, y) = T.break (=='\n') t
  in  case T.uncons y of
         Nothing -> if T.null x then [] else [x]
         Just (c, rest) -> (x <> T.singleton c) : breakLines rest

