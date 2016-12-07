{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad

import Test.Hspec

import Text.Commonmark.TestUtils.CMark
import Text.Commonmark.TestUtils.Spec

import Text.Commonmark.Html



main :: IO ()
main = hspec $
    describe "SpecTests" $
        forM_ spec $ \SpecTest{..} ->
            it (show testNumber ++ ": " ++ show testSection) $
                (docToHtml $ nodeToDoc $ commonmarkToNode [optNormalize] $ testIn)
                    `shouldBe` testOut
