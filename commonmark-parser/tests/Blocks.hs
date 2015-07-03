{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Blocks where

import           Test.Hspec

import           Text.CommonMark.Parser
import           Text.CommonMark.Parser.Options
import           Text.CommonMark.Syntax
import           Text.CommonMark.TestUtils.CMark
import           Text.CommonMark.TestUtils.Spec

import           Control.Monad
import           Data.Generics
import Unsafe.Coerce

import           Data.Maybe
import qualified Data.Text                       as T

testBlock :: Spec
testBlock = do
    describe "Block tests from specification" $ do
        forM_ blockTests $ \SpecTest{..} -> do
           it (show testNumber ++ ": " ++ show testSection) $
               normalizeDoc (commonmarkToDoc def {parseOptNormalize = True} testIn)
                   `shouldBe` normalizeDoc testOut

-- As CMark's AST doesn't preserve the character which was used to indicate
-- bullet lists, we need to change all Bullets to the same character
normalizeDoc :: Data a => Doc a -> Doc a
normalizeDoc = everywhere (mkT stripHtmlBlock) . everywhere (mkT changeBullet)
    where changeBullet (Bullet _) = Bullet Minus
          changeBullet a = a
          stripHtmlBlock (HtmlBlock t) = HtmlBlock $ T.strip t
          stripHtmlBlock a = a

blockTests :: [SpecTest T.Text (Doc T.Text)]
blockTests = unsafeCoerce $ filter (isNothing . docInline . testOut) $
                [t { testOut = nodeToDoc $ commonmarkToNode [optNormalize]
                                         $ testIn t } | t <- spec]
