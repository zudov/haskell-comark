module Main where

import           Blocks
import           Inline
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Control.DeepSeq
import           Data.Text                      (pack)

import           Text.CommonMark.Parser
import           Text.CommonMark.Parser.Options
import           Text.CommonMark.Syntax

main :: IO ()
main = do
    hspec $ do
        testInline
        testBlock

        describe "Properties" $ do
            prop "Any sequence of characters is a valid input"
                (\t -> commonmarkToDoc def (pack t) `deepseq` True)
