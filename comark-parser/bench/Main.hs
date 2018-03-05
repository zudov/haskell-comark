{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.Arrow      (second)
import           Criterion.Main
import           Data.FileEmbed
import           Data.Monoid
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text

import qualified CMark

import qualified Comark.Parser as Comark


samples :: [(FilePath, Text)]
samples = map (second Text.decodeUtf8) $(embedDir "bench/samples/")

main :: IO ()
main = defaultMain
  [ bgroup "pathological inputs" $ benches pathological
  , bgroup "markdown-it samples" $ benches samples
  ]

benches :: [(String,Text)] -> [Benchmark]
benches =
  concatMap $ \(n,c) ->
    [ bench ("cmark: " <> n) $ whnf (CMark.commonmarkToNode []) c
    , bench ("comark: " <> n) $ nf (Comark.parse []) c
    ]

pathological :: [(String, Text)]
pathological =
  [ ("nested brackets",    nested 50000 "[" "foo" "]")
  , ("nested parenthesis", nested 50000 "(" "foo" ")")
  ]

nested :: Int -> Text -> Text -> Text -> Text
nested n opener inner closer = Text.replicate n opener <> inner <> Text.replicate n closer
