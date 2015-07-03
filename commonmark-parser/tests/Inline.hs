{-# LANGUAGE RecordWildCards #-}
module Inline where

import           Data.Text                       (Text)
import           Test.Hspec

import           Data.Maybe

import           Text.CommonMark.Parser
import           Text.CommonMark.Parser.Options
import           Text.CommonMark.Syntax
import           Text.CommonMark.TestUtils.CMark
import           Text.CommonMark.TestUtils.Spec

import           Control.Monad
import           Data.Foldable                   (toList)
import           Unsafe.Coerce

testInline :: Spec
testInline = do
    describe "Inline tests from specification" $ do
        forM_ inlineTests $ \SpecTest{..} -> do
           it (show testNumber ++ ": " ++ show testSection) $ do
               let actual   = commonmarkToDoc def {parseOptNormalize = True} testIn
                   expected = nodeToDoc $ commonmarkToNode [optNormalize]
                                                           testIn
               case docInline (unsafeCoerce actual) of
                   Just is -> toList is `shouldBe`
                                    toList (fromJust (docInline expected))
                   Nothing -> unDoc actual `shouldBe` unDoc (unsafeCoerce expected)

inlineTests :: [SpecTest Text Text]
inlineTests = filter (isJust . docInline . nodeToDoc
                             . commonmarkToNode [] . testIn) spec

unDoc :: Doc Text -> [Block Text]
unDoc (Doc bs) = toList bs
