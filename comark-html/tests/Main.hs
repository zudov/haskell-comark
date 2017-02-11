{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad

import Test.Hspec

import Comark.TestUtils.CMark
import Comark.TestUtils.Spec

import Comark.Html



main :: IO ()
main = hspec $
    describe "SpecTests" $
        forM_ spec $ \SpecTest{..} ->
            it (show testNumber ++ ": " ++ show testSection) $
                (render $ nodeToDoc $ commonmarkToNode [optNormalize] $ testIn)
                    `shouldBe` testOut
