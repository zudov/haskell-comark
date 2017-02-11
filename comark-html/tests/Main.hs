{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad

import Test.Hspec

import Text.Comark.TestUtils.CMark
import Text.Comark.TestUtils.Spec

import Text.Comark.Html



main :: IO ()
main = hspec $
    describe "SpecTests" $
        forM_ spec $ \SpecTest{..} ->
            it (show testNumber ++ ": " ++ show testSection) $
                (render $ nodeToDoc $ commonmarkToNode [optNormalize] $ testIn)
                    `shouldBe` testOut
