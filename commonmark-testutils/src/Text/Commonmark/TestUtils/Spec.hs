{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Text.Commonmark.TestUtils.Spec
    ( SpecTest(..)
    , spec
    , docInline
    ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy              as B
import           Data.Maybe                        (fromJust)
import           Data.Sequence                     (ViewL(..), viewl)
import           Data.Text                         (Text)

import           Text.Commonmark.Syntax

import           Text.Commonmark.TestUtils.Spec.TH

spec :: [SpecTest Text Text]
spec = fromJust $ decode $ B.fromStrict $(specFile)

data SpecTest a b = SpecTest { testNumber  :: Int
                             , testSection :: Text
                             , testIn      :: a
                             , testOut     :: b
                             } deriving (Show, Eq)

instance FromJSON (SpecTest Text Text) where
    parseJSON = withObject "SpecTest" $ \o ->
        SpecTest <$> o .: "example"
                 <*> o .: "section"
                 <*> o .: "markdown"
                 <*> o .: "html"

-- | Some tests are specifically targeted to test inline parsing.
--   Such tests consist of one paragraph with inline elements.
--   This function extracts sequence of inline elements from such tests.
docInline :: Doc a -> Maybe (Inlines a)
docInline (Doc (viewl -> (Para is :< (viewl -> EmptyL)))) = Just is
docInline _ = Nothing

