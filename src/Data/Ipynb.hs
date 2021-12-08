{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{- |
   Module      : Data.Ipynb
   Copyright   : Copyright (C) 2019 John MacFarlane
   License     : BSD3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Data structure and JSON serializers for ipynb (Jupyter notebook) format.
Version 4 of the format is documented here:
<https://nbformat.readthedocs.io/en/latest/format_description.html>.

The library supports both version 4 ('Notebook NbV4') and version 3
('Notebook NbV3') of nbformat.  Note that this is a phantom type: the
`NbV3` or `NbV4` parameter only affects JSON serialization,
not the data structure itself.  So code that manipulates
notebooks can be polymorphic, operating on `Notebook a`.

-}
module Data.Ipynb ( Notebook(..)
                  , NbV3
                  , NbV4
                  , JSONMeta(..)
                  , Cell(..)
                  , Source(..)
                  , CellType(..)
                  , Output(..)
                  , MimeType
                  , MimeData(..)
                  , MimeBundle(..)
                  , MimeAttachments(..)
                  , breakLines
                  )
where
import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import Data.Char (isSpace)
import Data.List (partition, sortOn)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics
import Prelude
#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup
#endif
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KM
#else
import qualified Data.HashMap.Strict as KM
#endif
import Data.String
import qualified Data.Set as Set

-- | Indexes 'Notebook' for serialization as nbformat version 3.
data NbV3

-- | Indexes 'Notebook' for serialization as nbformat version 4.
data NbV4

-- | A Jupyter notebook.
data Notebook a = Notebook
  { notebookMetadata :: JSONMeta
  , notebookFormat   :: (Int, Int)
  , notebookCells    :: [Cell a]
  } deriving (Show, Eq, Generic)

instance Semigroup (Notebook a) where
  Notebook m1 f1 c1 <> Notebook m2 f2 c2 =
    Notebook (m1 <> m2) (max f1 f2) (c1 <> c2)

instance Monoid (Notebook a) where
  mempty = Notebook mempty (0, 0) mempty
#if MIN_VERSION_base(4,11,0)
#else
  mappend = (<>)
#endif

instance FromJSON (Notebook NbV4) where
  parseJSON = withObject "Notebook" $ \v -> do
    fmt <- v .:? "nbformat" .!= 0
    when (fmt < 4 || fmt > 4) $ fail "expected nbformat == 4"
    fmtminor <- v .:? "nbformat_minor" .!= 0
    metadata <- v .:? "metadata" .!= mempty
    cells <- v .: "cells"
    return
      Notebook{ notebookMetadata = metadata
              , notebookFormat = (fmt, fmtminor)
              , notebookCells    = cells
              }

instance FromJSON (Notebook NbV3) where
  parseJSON = withObject "Notebook" $ \v -> do
    fmt <- v .:? "nbformat" .!= 0
    when (fmt > 3) $ fail "expected nbformat <= 3"
    fmtminor <- v .:? "nbformat_minor" .!= 0
    metadata <- v .:? "metadata" .!= mempty
    worksheets <- v .: "worksheets"
    -- NOTE: we ignore metadata on worksheets: is this ever used?
    cells <- mconcat <$> mapM (.: "cells") worksheets
    return
      Notebook{ notebookMetadata = metadata
              , notebookFormat = (fmt, fmtminor)
              , notebookCells = cells
              }

instance ToJSON (Notebook NbV4) where
 toJSON n = object
   [ "nbformat" .= fst (notebookFormat n)
   , "nbformat_minor" .= snd (notebookFormat n)
   , "metadata" .= notebookMetadata n
   , "cells" .= (if notebookFormat n >= (4,1)
                    then id
                    else map (\c -> c{ cellAttachments = Nothing }))
                (notebookCells n)
   ]

instance ToJSON (Notebook NbV3) where
 toJSON n = object
   [ "nbformat" .= fst (notebookFormat n)
   , "nbformat_minor" .= snd (notebookFormat n)
   , "metadata" .= notebookMetadata n
   , "worksheets" .=
     [ object
       [ "cells" .= (if notebookFormat n >= (4,1)
                        then id
                        else map (\c -> c{ cellAttachments = Nothing }))
                    (notebookCells n)
       , "metadata" .= (mempty :: JSONMeta) -- see above in FromJSON instance
       ]
     ]
   ]

newtype JSONMeta = JSONMeta (M.Map Text Value)
  deriving (Show, Eq, Ord, Generic, Semigroup, Monoid, FromJSON)

instance ToJSON JSONMeta where
   toJSON = genericToJSON defaultOptions
   toEncoding = genericToEncoding defaultOptions

-- | A 'Source' is a textual content which may be
-- represented in JSON either as a single string
-- or as a list of strings (which are concatenated).
newtype Source = Source{ unSource :: [Text] }
  deriving (Show, Eq, Generic, Semigroup, Monoid)

instance FromJSON Source where
  parseJSON v = do
    ts <- parseJSON v <|> (:[]) <$> parseJSON v
    return $ Source ts

instance ToJSON Source where
  toJSON (Source ts) = toJSON ts

newtype MimeAttachments = MimeAttachments (M.Map Text MimeBundle)
  deriving (Show, Eq, Ord, Generic, Semigroup, Monoid, FromJSON)

instance ToJSON MimeAttachments where
   toJSON = genericToJSON defaultOptions
   toEncoding = genericToEncoding defaultOptions

-- | A Jupyter notebook cell.
data Cell a = Cell
  { cellType        :: CellType a
  , cellId          :: Maybe Text
  , cellSource      :: Source
  , cellMetadata    :: JSONMeta
  , cellAttachments :: Maybe MimeAttachments
} deriving (Show, Eq, Generic)

instance FromJSON (Cell NbV4) where
  parseJSON = withObject "Cell" $ \v -> do
    ty <- v .: "cell_type"
    cell_id <- v.:? "id"
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
      Cell{ cellType = cell_type
          , cellId = cell_id
          , cellMetadata = metadata
          , cellAttachments = attachments
          , cellSource = source
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
    source <- if ty == "code"
                 then v .: "input"
                 else v .: "source"
    return
      Cell{ cellType = cell_type
          , cellId = Nothing
          , cellMetadata = metadata
          , cellAttachments = Nothing
          , cellSource = source
          }

-- note that execution_count can't be omitted!
instance ToJSON (Cell NbV4) where
 toJSON c = object $
   ("metadata" .= cellMetadata c) :
   maybe [] (\x -> ["id" .= cellId c]) (cellId c) ++
   maybe [] (\x -> ["attachments" .= x]) (cellAttachments c) ++
   case cellType c of
     Markdown -> [ "cell_type" .= ("markdown" :: Text)
                 , "source" .= cellSource c ]
     Heading lev ->
                [ "cell_type" .= ("markdown" :: Text)
                , "source" .=
                     (Source . breakLines .
                      ((T.replicate lev "#" <> " ") <>) .
                      mconcat . unSource) (cellSource c)
                 ]
     Raw      -> [ "cell_type" .= ("raw" :: Text)
                 , "source" .= cellSource c
                 ]
     Code{
         codeExecutionCount = ec
       , codeOutputs = outs
       }      -> [ "cell_type" .= ("code" :: Text)
                 , "execution_count" .= ec
                 , "outputs" .= outs
                 , "source" .= cellSource c
                 ]

instance ToJSON (Cell NbV3) where
 toJSON c =
  object $
   metadataToV3Pairs (cellMetadata c) ++
   case cellType c of
     Markdown    -> [ "cell_type" .= ("markdown" :: Text)
                    , "source" .= cellSource c
                    ]
     Heading lev -> [ "cell_type" .= ("heading" :: Text)
                    , "level" .= lev
                    , "source" .= cellSource c
                    ]
     Raw         -> [ "cell_type" .= ("raw" :: Text)
                    , "source" .= cellSource c
                    ]
     Code{
         codeExecutionCount = ec
       , codeOutputs = outs
       }      -> [ "cell_type" .= ("code" :: Text)
                 , "input" .= cellSource c
                 , "outputs" .= outs
                 ] ++
                 maybe [] (\n -> ["prompt_number" .= n]) ec

-- in v3, certain metadata fields occur in the main cell object.
-- e.g. collapsed, language.
metadataToV3Pairs :: JSONMeta -> [Aeson.Pair]
metadataToV3Pairs (JSONMeta meta) =
  ("metadata" .= JSONMeta (M.fromList regMeta)) : map toPair extraMeta
  where (extraMeta, regMeta) = partition isExtraMeta $ M.toList meta
        toPair (k,v) = (fromString (T.unpack k)) .= v
        isExtraMeta (k,_) = k `Set.member` v3MetaInMainCell

v3MetaInMainCell :: Set.Set Text
v3MetaInMainCell = Set.fromList ["collapsed", "language"]

parseV3Metadata :: Aeson.Object -> Aeson.Parser JSONMeta
parseV3Metadata v = do
  meta <- v .:? "metadata" .!= mempty
  vm <- parseJSON (Object v)
  let extraMeta = JSONMeta (M.restrictKeys vm v3MetaInMainCell)
  return (meta <> extraMeta)

-- | Information about the type of a notebook cell, plus
-- data specific to that type.  note that 'Heading' is
-- for v3 only; a 'Heading' will be rendered as 'Markdown'
-- in v4.
data CellType a =
    Markdown
  | Heading -- V3 only
    { headingLevel  :: Int
    }
  | Raw
  | Code
    { codeExecutionCount :: Maybe Int
    , codeOutputs        :: [Output a]
    }
  deriving (Show, Eq, Generic)

-- | Output from a Code cell.
data Output a =
    Stream
    { streamName :: Text
    , streamText :: Source }
  | DisplayData
    { displayData     :: MimeBundle
    , displayMetadata :: JSONMeta
    }
  | ExecuteResult
    { executeCount    :: Int
    , executeData     :: MimeBundle
    , executeMetadata :: JSONMeta
    }
  | Err
    { errName      :: Text
    , errValue     :: Text
    , errTraceback :: [Text]
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
        DisplayData
          <$> v .: "data"
          <*> v .:? "metadata" .!= mempty
      "execute_result" ->
        ExecuteResult
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
        DisplayData
          <$> extractNbV3Data v
          <*> v .:? "metadata" .!= mempty
      "pyout" ->
        ExecuteResult
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
  let go ("output_type", _)   = Nothing
      go ("metadata", _)      = Nothing
      go ("prompt_number", _) = Nothing
      go ("text", x)          = Just ("text/plain", x)
      go ("latex", x)         = Just ("text/latex", x)
      go ("html", x)          = Just ("text/html", x)
      go ("png", x)           = Just ("image/png", x)
      go ("jpeg", x)          = Just ("image/jpeg", x)
      go ("javascript", x)    = Just ("application/javascript", x)
      go (_, _)               = Nothing -- TODO complete list? where documented?
  parseJSON (Object . KM.fromList . mapMaybe go . KM.toList $ v)

instance ToJSON (Output NbV4) where
  toJSON s@Stream{} = object
    [ "output_type" .= ("stream" :: Text)
    , "name" .= streamName s
    , "text" .= streamText s
    ]
  toJSON d@DisplayData{} = object
    [ "output_type" .= ("display_data" :: Text)
    , "data" .= displayData d
    , "metadata" .= displayMetadata d
    ]
  toJSON e@ExecuteResult{} = object
    [ "output_type" .= ("execute_result" :: Text)
    , "execution_count" .= executeCount e
    , "data" .= executeData e
    , "metadata" .= executeMetadata e
    ]
  toJSON e@Err{} = object
    [ "output_type" .= ("error" :: Text)
    , "ename" .= errName e
    , "evalue" .= errValue e
    , "traceback" .= errTraceback e
    ]

instance ToJSON (Output NbV3) where
  toJSON s@Stream{} = object
    [ "output_type" .= ("stream" :: Text)
    , "stream" .= streamName s
    , "text" .= streamText s
    ]
  toJSON d@DisplayData{} =
    adjustV3DataFields $ object
    [ "output_type" .= ("display_data" :: Text)
    , "data" .= displayData d
    , "metadata" .= displayMetadata d ]
  toJSON e@ExecuteResult{} =
    adjustV3DataFields $ object
    [ "output_type" .= ("pyout" :: Text)
    , "prompt_number" .= executeCount e
    , "data" .= executeData e
    , "metadata" .= executeMetadata e ]
  toJSON e@Err{} = object
    [ "output_type" .= ("pyerr" :: Text)
    , "ename" .= errName e
    , "evalue" .= errValue e
    , "traceback" .= errTraceback e
    ]

adjustV3DataFields :: Value -> Value
adjustV3DataFields (Object hm) =
  case KM.lookup "data" hm of
    Just (Object dm) -> Object $
      KM.delete "data" $ foldr
      (\(k, v) -> KM.insert (modKey k) v) hm
      (KM.toList dm)
    _ -> Object hm
  where  modKey "text/plain"             = "text"
         modKey "text/latex"             = "latex"
         modKey "text/html"              = "html"
         modKey "image/jpeg"             = "jpeg"
         modKey "image/png"              = "png"
         modKey "application/javascript" = "javascript"
         modKey x                        = x
adjustV3DataFields x = x

-- | Data in an execution result or display data cell.
data MimeData =
    BinaryData ByteString
  | TextualData Text
  | JsonData Value
  deriving (Show, Eq, Ord, Generic)

instance ToJSON MimeData where
  toJSON = toJSON . mimeDataToValue
  toEncoding = toEncoding . mimeDataToValue

mimeDataToValue :: MimeData -> Value
mimeDataToValue (BinaryData bs) =
  toJSON .
    TE.decodeUtf8 .
    (<> "\n") .
    B.intercalate "\n" .  chunksOf 76 .
    Base64.encode
    $ bs
mimeDataToValue (JsonData v) = v
mimeDataToValue (TextualData t) = toJSON (breakLines t)

type MimeType = Text

-- | A 'MimeBundle' wraps a map from mime types to mime data.
newtype MimeBundle = MimeBundle{ unMimeBundle :: M.Map MimeType MimeData }
  deriving (Show, Eq, Ord, Generic, Semigroup, Monoid)

instance FromJSON MimeBundle where
  parseJSON v = do
    m <- parseJSON v >>= mapM pairToMimeData . M.toList
    return $ MimeBundle $ M.fromList m

pairToMimeData :: (MimeType, Value) -> Aeson.Parser (MimeType, MimeData)
pairToMimeData (mt, v)
  | mt == "application/json" ||
    "+json" `T.isSuffixOf` mt = return (mt, JsonData v)
pairToMimeData (mt, v) = do
  t <- parseJSON v <|> (mconcat <$> parseJSON v)
  let mimeprefix = T.takeWhile (/='/') mt
  if mimeprefix == "text"
     then return (mt, TextualData t)
     else
       case Base64.decode (TE.encodeUtf8 (T.filter (not . isSpace) t)) of
            Left _  -> return (mt, TextualData t)
            Right b -> return (mt, BinaryData b)

instance ToJSON MimeBundle where
  toJSON (MimeBundle m) =
    toJSON m
  toEncoding (MimeBundle m) =  -- ensure deterministic (sorted) order
    pairs $ mconcat $ map (\(k,v) -> (fromString (T.unpack k) Aeson..= v))
                          (sortOn fst (M.toList m))

chunksOf :: Int -> ByteString -> [ByteString]
chunksOf k s
   | B.null s = []
   | otherwise =
     let (h,t) = B.splitAt k s
     in h : chunksOf k t

-- | Break up a string into a list of strings, each representing
-- one line of the string (including trailing newline if any).
breakLines :: Text -> [Text]
breakLines t =
  let (x, y) = T.break (=='\n') t
  in  case T.uncons y of
         Nothing        -> if T.null x then [] else [x]
         Just (c, rest) -> (x <> T.singleton c) : breakLines rest

