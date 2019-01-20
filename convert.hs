-- A little conversion program for testing.
-- Run with
-- stack runghc --package aeson-pretty convert.hs -- my.ipynb

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.Ipynb
import Data.ByteString.Lazy as LB
import Data.Aeson.Encode.Pretty (Config(..), defConfig,
           encodePretty', keyOrder, Indent(Spaces))
import Data.Aeson
import System.Environment

main = do
  [fp] <- getArgs
  bs <- LB.readFile fp
  let Just (notebook :: Notebook NbV4) = decode bs
  LB.putStr $ encodePretty' defConfig{
             confIndent  = Spaces 1,
             confCompare = keyOrder
               [ "cells", "nbformat", "nbformat_minor",
                 "cell_type", "output_type",
                 "execution_count", "metadata",
                 "outputs", "source",
                 "data", "name", "text" ] } $ notebook
