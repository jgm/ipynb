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
                  , encodeNotebook
                  )
where
import Prelude
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty (Config(..), defConfig, encodePretty',
                                 keyOrder, Indent(Spaces))
import qualified Data.Aeson.Types as Aeson
import Control.Applicative ((<|>))
import qualified Data.ByteString.Base64 as Base64
import GHC.Generics
import Control.Monad (when)

type MimeType = Text

encodeNotebook :: ToJSON (Notebook a) => Notebook a -> Text
encodeNotebook = TE.decodeUtf8 . BL.toStrict .
  encodePretty' defConfig{
      confIndent  = Spaces 1,
      confCompare = keyOrder
           [ "cells", "nbformat", "nbformat_minor",
             "cell_type", "output_type",
             "execution_count", "metadata",
             "outputs", "source",
             "data", "name", "text" ] }

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
    when (fmt < 3 || fmt > 3) $ fail $ "expected nbformat == 3"
    fmtminor <- v .:? "nbformat_minor" .!= 0
    metadata <- v .:? "metadata" .!= mempty
    worksheets <- v .: "worksheets"
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
            <$> v .: "execution_count"
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
        "heading" -> pure Markdown
        "raw" -> pure Raw
        "code" ->
          Code
            <$> v .: "prompt_number"
            <*> v .: "outputs"
        _ -> fail $ "Unknown cell_type " ++ ty
    metadata <- v .: "metadata"
    attachments <- v .:? "attachments"
    source <- if ty == "code"
                 then v .: "input"
                 else v .: "source"
    source' <- if ty == "heading"
                  then do
                    (level :: Int) <- v .: "level"
                    let hashes = T.replicate level "#"
                    -- parse heading as regular markdown cell
                    return $ Source $ breakLines
                           $ hashes <> " " <> mconcat (unSource source)
                  else pure source
    return
      Cell{ c_cell_type = cell_type
          , c_metadata = metadata
          , c_attachments = attachments
          , c_source = source'
          }

-- note that execution_count can't be omitted!
instance ToJSON (Cell NbV4) where
 toJSON c = object $
   [ "source" .= (c_source c)
   , "metadata" .= (c_metadata c)
   ] ++
   maybe [] (\x -> ["attachments" .= x]) (c_attachments c) ++
   case c_cell_type c of
     Markdown -> [ "cell_type" .= ("markdown" :: Text) ]
     Raw      -> [ "cell_type" .= ("raw" :: Text) ]
     Code{
         c_execution_count = ec
       , c_outputs = outs
       }      -> [ "cell_type" .= ("code" :: Text)
                 , "execution_count" .= ec
                 , "outputs" .= outs
                 ]

data CellType a =
    Markdown
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
      go ("png", x)  = Just ("image/png", x)
      go ("jpg", x)  = Just ("image/jpeg", x)
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
  let mimeprefix = T.takeWhile (/='/') mt
  if mimeprefix == "image" || mimeprefix == "video"
     then do
       t <- parseJSON v <|> (mconcat <$> parseJSON v)
       return (mt, BinaryData (Base64.decodeLenient . TE.encodeUtf8 $ t))
     else do
       t <- parseJSON v <|> (mconcat <$> parseJSON v)
       return $ (mt, TextualData t)

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

{- --- for testing only:
import qualified Data.ByteString.Lazy as BL

readNotebookFile :: FilePath -> IO Notebook
readNotebookFile fp = do
  bs <- BL.readFile fp
  case eitherDecode bs of
    Right nb -> return nb
    Left err -> error err

writeNotebookFile :: FilePath -> Notebook -> IO ()
writeNotebookFile fp = BL.writeFile fp . encode
-}
