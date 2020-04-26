{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Aeson (Value (..), eitherDecode, encode)
import Data.Aeson.Diff as AesonDiff
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import Data.Ipynb
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
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

-- We don't want tests failing because of inconsequential
-- differences in formatting of base64 data, like line breaks.
normalizeBase64 :: Value -> Value
normalizeBase64 bs =
  bs & key "cells" . values . key "outputs" . values . key "data"
       . _Object %~ HM.mapWithKey (\k v ->
                       if k == "application/json" ||
                          "text/" `T.isPrefixOf` k ||
                          "+json" `T.isSuffixOf` k
                          then v
                          else go v)
  where
     go (Array vec) =
       go $ String
          $ mconcat $ map
              (\v' -> case v' of
                        String t' -> t'
                        _         -> error "expected String") $ V.toList vec
     go (String t) =
       case Base64.decode (TE.encodeUtf8 (T.filter (/='\n') t)) of
            Left _  -> String t  -- textual
            Right b -> String $
              TE.decodeUtf8 .  (<> "\n") .
              B.intercalate "\n" .  chunksOf 76 .
              Base64.encode $ b
     go v = v

chunksOf :: Int -> B.ByteString -> [B.ByteString]
chunksOf k s
   | B.null s = []
   | otherwise =
     let (h,t) = B.splitAt k s
     in h : chunksOf k t

rtTest :: FilePath -> TestTree
rtTest fp = testCase fp $ do
  inRaw <- BL.readFile fp
  let format = inRaw ^? key "nbformat"._Number
  case format of
    Just 4 -> rtTest4 inRaw
    _      -> rtTest3 inRaw

rtTest3 :: BL.ByteString -> IO ()
rtTest3 inRaw = do
  (inJSON :: Value) <- either error return $ eitherDecode inRaw
  (nb :: Notebook NbV3) <- either error return $ eitherDecode inRaw
  let outRaw = encode nb
  (nb' :: Notebook NbV3) <- either error return $ eitherDecode outRaw
  (outJSON :: Value) <- either error return $ eitherDecode outRaw
  -- test that (read . write) == id
  let patch' = AesonDiff.diff
         (normalizeBase64 inJSON) (normalizeBase64 outJSON)
  assertBool (show patch') (patch' == Patch [])
  -- now test that (write . read) == id
  assertEqual "write . read != read" nb nb'

rtTest4 :: BL.ByteString -> IO ()
rtTest4 inRaw = do
  (inJSON :: Value) <- either error return $ eitherDecode inRaw
  (nb :: Notebook NbV4) <- either error return $ eitherDecode inRaw
  let outRaw = encode nb
  (nb' :: Notebook NbV4) <- either error return $ eitherDecode outRaw
  (outJSON :: Value) <- either error return $ eitherDecode outRaw
  -- test that (read . write) == id
  let patch' = AesonDiff.diff
       (normalizeBase64 inJSON) (normalizeBase64 outJSON)
  assertBool (show patch') (patch' == Patch [])
  -- now test that (write . read) == id
  assertEqual "write . read != read" nb nb'

