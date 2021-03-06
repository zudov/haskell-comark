module Main where

import Blocks
import Inline
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Control.DeepSeq
import Data.Text       (pack)

import Comark.Parser
import Comark.Parser.Options
import Comark.Syntax

main :: IO ()
main = do
    hspec $ do
        testInline
        testBlock

        describe "Properties" $ do
            prop "Any sequence of characters is a valid input"
              (\t -> parse [] (pack t) `deepseq` True)
