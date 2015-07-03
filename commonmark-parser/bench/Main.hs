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

import           Text.CommonMark.Parser
import           Text.CommonMark.Parser.Inline
import           Text.CommonMark.Parser.Options
import           Text.CommonMark.Syntax

samples :: [(FilePath, Text)]
samples = map (fmap decodeUtf8) $(embedDir "bench/samples/")

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight (Left _) = error "fromRight"

main :: IO ()
main = defaultMain
  [ bgroup "pathological with normalization" $ benches True pathological
  , bgroup "pathological without normalization" $ benches False pathological
  ]

benches norm = map (\(n,c) -> bench n $ nf (commonmarkToDoc def {parseOptNormalize = norm}) c)

pathological :: [(String, Text)]
pathological =
  [ ("nested brackets", nested 50000 "[" "foo" "]")]
  {-, ("nested parenthesis", nested 10000 "(" "foo" ")") ]-}

nested :: Int -> Text -> Text -> Text -> Text
nested n opener inner closer = T.replicate n opener <> inner <> T.replicate n closer
