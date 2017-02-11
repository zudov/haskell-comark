{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where


import Text.Commonmark.Parser
import Text.Commonmark.Parser.Options
import Text.Commonmark.Syntax
import           Control.Arrow      (second)
import           Control.DeepSeq
import           Criterion.Main
import           Data.FileEmbed
import           Data.Foldable
import           Data.Monoid
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text

samples :: [(FilePath, Text)]
samples = map (fmap decodeUtf8) $(embedDir "bench/samples/")

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight (Left _) = error "fromRight"

main :: IO ()
main = defaultMain
  [ bgroup "pathological with normalization"           $ benches [Normalize] pathological
  , bgroup "pathological without normalization"        $ benches [] pathological
  , bgroup "markdown-it samples with normalization"    $ benches [Normalize]  samples
  , bgroup "markdown-it samples without normalization" $ benches [] samples
  ]

benches opts = map (\(n,c) -> bench n $ nf (commonmarkToDoc opts) c)

pathological :: [(String, Text)]
pathological =
  [ ("nested brackets",    nested 50000 "[" "foo" "]")
  , ("nested parenthesis", nested 50000 "(" "foo" ")")
  ]

nested :: Int -> Text -> Text -> Text -> Text
nested n opener inner closer = T.replicate n opener <> inner <> T.replicate n closer
