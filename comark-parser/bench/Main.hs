{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.Arrow      (second)
import           Control.DeepSeq
import           Criterion.Main
import           Data.FileEmbed
import           Data.Foldable
import           Data.Monoid
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text

import qualified Text.Comark.Parser         as Comark
import qualified Text.Comark.Parser.Options as Comark
import qualified Text.Comark.Syntax         as Comark

samples :: [(FilePath, Text)]
samples = map (second Text.decodeUtf8) $(embedDir "bench/samples/")

main :: IO ()
main = defaultMain
  [ bgroup "pathological with normalization"           $ benches [Comark.Normalize] pathological
  , bgroup "pathological without normalization"        $ benches [] pathological
  , bgroup "markdown-it samples with normalization"    $ benches [Comark.Normalize] samples
  , bgroup "markdown-it samples without normalization" $ benches [] samples
  ]

benches opts = map (\(n,c) -> bench n $ nf (Comark.parse opts) c)

pathological :: [(String, Text)]
pathological =
  [ ("nested brackets",    nested 50000 "[" "foo" "]")
  , ("nested parenthesis", nested 50000 "(" "foo" ")")
  ]

nested :: Int -> Text -> Text -> Text -> Text
nested n opener inner closer = Text.replicate n opener <> inner <> Text.replicate n closer
