{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.DeepSeq
import           Criterion.Main
import           Data.FileEmbed
import           Data.Foldable
import           Data.Monoid
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.Encoding
import           Data.Text.Lazy                 (toStrict)
import           Data.Text.Lazy.Builder

import           Text.Commonmark.Parser
import           Text.Commonmark.Parser.Options
import           Text.Commonmark.Syntax

samples :: [(FilePath, Text)]
samples = map (fmap decodeUtf8) $(embedDir "bench/samples/")

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight (Left _) = error "fromRight"

main :: IO ()
main = defaultMain
  [ bgroup "pathological with normalization"           $ benches True  pathological
  , bgroup "pathological without normalization"        $ benches False pathological
  , bgroup "markdown-it samples with normalization"    $ benches True  samples
  , bgroup "markdown-it samples without normalization" $ benches False samples
  ]

benches norm = map (\(n,c) -> bench n $ nf (commonmarkToDoc defParseOptions {parseOptNormalize = norm}) c)

pathological :: [(String, Text)]
pathological =
  [ ("nested brackets",    nested 50000 "[" "foo" "]")
  , ("nested parenthesis", nested 50000 "(" "foo" ")")
  ]

nested :: Int -> Text -> Text -> Text -> Text
nested n opener inner closer = T.replicate n opener <> inner <> T.replicate n closer
