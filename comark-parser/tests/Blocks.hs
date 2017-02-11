{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Blocks where

import Test.Hspec

import Comark.Parser
import Comark.Syntax
import Comark.TestUtils.CMark
import Comark.TestUtils.Spec

import Control.Monad
import Data.Generics
import Unsafe.Coerce

import           Data.Maybe
import           Data.Text  (Text)
import qualified Data.Text  as Text

testBlock :: Spec
testBlock = do
    describe "Block tests from specification" $ do
        forM_ blockTests $ \SpecTest{..} -> do
           it (show testNumber ++ ": " ++ show testSection) $
               normalizeDoc (parse [Normalize] testIn)
                 `shouldBe` normalizeDoc testOut

-- As CMark's AST doesn't preserve the character which was used to indicate
-- bullet lists, we need to change all Bullets to the same character
normalizeDoc :: Data a => Doc a -> Doc a
normalizeDoc = everywhere (mkT stripHtmlBlock) . everywhere (mkT changeBullet)
  where
    changeBullet (Bullet _) = Bullet Minus
    changeBullet a          = a
    stripHtmlBlock (HtmlBlock t) = HtmlBlock $ Text.strip t
    stripHtmlBlock a             = a

blockTests :: [SpecTest Text (Doc Text)]
blockTests =
  unsafeCoerce $ filter (isNothing . docInline . testOut) $
    [ t { testOut = nodeToDoc $ commonmarkToNode [optNormalize] $ testIn t }
    | t <- spec
    ]
