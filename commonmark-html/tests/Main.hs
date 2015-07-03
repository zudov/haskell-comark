{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Monad

import           Test.Hspec

import           Text.CommonMark.TestUtils.CMark
import           Text.CommonMark.TestUtils.Spec

import           Text.CommonMark.Html



main :: IO ()
main = hspec $
    describe "SpecTests" $
        forM_ spec $ \SpecTest{..} ->
            it (show testNumber ++ ": " ++ show testSection) $
                (docToHtml $ nodeToDoc $ commonmarkToNode [optNormalize] $ testIn)
                    `shouldBe` testOut
