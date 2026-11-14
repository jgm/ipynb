{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (unless)
import Data.Aeson (Value (..), eitherDecode, encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Ipynb
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import Lens.Micro
import Lens.Micro.Aeson
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Data.Monoid

main :: IO ()
main = do
  let rtdir = "test" </> "rt-files"
  createDirectoryIfMissing False rtdir
  fs <- map (rtdir </>) . filter isIpynb <$> getDirectoryContents rtdir
  defaultMain $ testGroup "tests"
    [ testGroup "round-trip tests" $ map rtTest fs
    , testGroup "mime data tests" mimeTests
    ]

-- Parse a v4 notebook with a single code cell whose only output
-- is display_data with the given data object, returning the MimeBundle.
parseV4Data :: BL.ByteString -> Either String MimeBundle
parseV4Data dat = do
  (nb :: Notebook NbV4) <- eitherDecode $
    "{\"nbformat\":4,\"nbformat_minor\":5,\"metadata\":{},\"cells\":" <>
    "[{\"cell_type\":\"code\",\"metadata\":{},\"source\":\"\"," <>
    "\"execution_count\":1,\"outputs\":[{\"output_type\":\"display_data\"," <>
    "\"metadata\":{},\"data\":" <> dat <> "}]}]}"
  case map cellType (notebookCells nb) of
    [Code{ codeOutputs = [DisplayData{ displayData = d }] }] -> Right d
    _ -> Left "unexpected notebook structure"

mimeTests :: [TestTree]
mimeTests =
  [ testCase "textual data that looks like base64 stays textual" $
      parseV4Data "{\"application/javascript\":\"test\"}" @?=
        Right (MimeBundle (M.fromList
          [("application/javascript", TextualData "test")]))
  , testCase "image data is decoded from base64" $
      parseV4Data "{\"image/png\":\"dGVzdA==\\n\"}" @?=
        Right (MimeBundle (M.fromList [("image/png", BinaryData "test")]))
  , testCase "svg data stays textual" $
      parseV4Data "{\"image/svg+xml\":[\"<svg>\\n\",\"</svg>\"]}" @?=
        Right (MimeBundle (M.fromList
          [("image/svg+xml", TextualData "<svg>\n</svg>")]))
  , testCase "non-textual data under a non-json mime type is kept as json" $
      parseV4Data "{\"application/vnd.foo\":{\"a\":1}}" @?=
        Right (MimeBundle (M.fromList
          [("application/vnd.foo", JsonData (object ["a" .= (1 :: Int)]))]))
  ]

isIpynb :: FilePath -> Bool
isIpynb fp = takeExtension fp == ".ipynb"

rtTest :: FilePath -> TestTree
rtTest fp = testCase fp $ do
  inRaw <- BL.readFile fp
  let format = inRaw ^? key "nbformat" . _Number
  case format of
    Just 4 -> rtTest4 inRaw
    _      -> rtTest3 inRaw

rtTest3 :: BL.ByteString -> IO ()
rtTest3 inRaw = do
  (nb :: Notebook NbV3) <- either error return $ eitherDecode inRaw
  let outRaw = encode nb
  (nb' :: Notebook NbV3) <- either error return $ eitherDecode outRaw
  -- test that (write . read) == id
  assertEqual "write . read != read" nb nb'

rtTest4 :: BL.ByteString -> IO ()
rtTest4 inRaw = do
  (nb :: Notebook NbV4) <- either error return $ eitherDecode inRaw
  let outRaw = encode nb
  (nb' :: Notebook NbV4) <- either error return $ eitherDecode outRaw
  -- test that (write . read) == id
  assertEqual "write . read != read" nb nb'

