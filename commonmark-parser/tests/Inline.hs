{-# LANGUAGE RecordWildCards #-}
module Inline where

import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Test.Hspec

import           Data.Maybe
import qualified Data.Sequence                   as Seq

import           Text.Commonmark.Parser
import           Text.Commonmark.Parser.Options
import           Text.Commonmark.Syntax
import           Text.Commonmark.TestUtils.CMark
import           Text.Commonmark.TestUtils.Spec

import           Control.Monad
import           Data.Foldable                   (toList)
import           Unsafe.Coerce

testInline :: Spec
testInline = do
    describe "Inline tests from specification" $ do
        forM_ inlineTests $ \SpecTest{..} -> do
           it (show testNumber ++ ": " ++ show testSection) $ do
               let actual   = commonmarkToDoc defParseOptions {parseOptNormalize = True} testIn
                   expected = normalizeDoc $ nodeToDoc $ commonmarkToNode [optNormalize]
                                                                          testIn
               case docInline (unsafeCoerce actual) of
                   Just is -> toList is `shouldBe`
                                    toList (fromJust (docInline expected))
                   Nothing -> unDoc actual `shouldBe` unDoc (unsafeCoerce expected)

inlineTests :: [SpecTest Text Text]
inlineTests = filter (isJust . docInline . nodeToDoc
                             . commonmarkToNode [] . testIn) spec

normalizeDoc :: Doc Text -> Doc Text
normalizeDoc (Doc blocks) = Doc $ fmap m blocks
  where
    m (Heading a b) = Heading a $ (dropEmptyStrs . normalize) b
    m (Para a) = Para $ (dropEmptyStrs . normalize) a
    m (Quote a) = Quote $ fmap m a
    m (List a b c) = List a b $ fmap (fmap m) c
    m a = a

    dropEmptyStrs = Seq.filter (not . isEmptyStr)

    isEmptyStr (Str a) = Text.null a
    isEmptyStr _ = False

unDoc :: Doc Text -> [Block Text]
unDoc (Doc bs) = toList bs
