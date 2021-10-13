{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (unless)
import Data.Aeson (Value (..), eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Ipynb
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
  defaultMain $ testGroup "round-trip tests" $ map rtTest fs

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

