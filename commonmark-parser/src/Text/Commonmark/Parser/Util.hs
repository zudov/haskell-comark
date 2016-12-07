{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Text.Commonmark.Parser.Util
  ( Scanner ()
  , isLineEnding
  , pLineEnding
  , isWhitespace
  , pWhitespace
  , pSpaces
  , pSpacesUpToColumn
  , pIndentSpaces
  , pNonIndentSpaces
  , pBlankline
  , isUnicodeWhitespace
  , isAsciiPunctuation
  , satisfyUpTo
  , lookupLinkReference
  , normalizeReference
  , parenthesize
  ) where

import           Control.Applicative
import           Control.Bool
import           Control.Monad
import           Data.Char
import qualified Data.Map            as Map
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text.Extended  as Text
import           Prelude             hiding (takeWhile)

import Text.Commonmark.ParserCombinators
import Text.Commonmark.Types

type Scanner = Parser ()

-- | Predicate for line ending character (newline or carriage return).
--   NB: something like `satisfy isLineEnding` won't properly parse a
--   line ending, when given '\r\n' as input it would just consume '\r'
--   leaving '\n' unconsumed. In such cases one should use 'pLineEnding'
--   instead
isLineEnding :: Char -> Bool
isLineEnding = (== '\r') <||> (== '\n')

-- | A newline (U+000A), carriage return (U+000D), or carriage return followed by newline.
pLineEnding :: Parser Text
pLineEnding = "\n" <|> "\r\n" <|> "\r"

-- | A [whitespace character] as in spec
isWhitespace :: Char -> Bool
isWhitespace = (`elem` (" \t\n\r\f\v" :: [Char]))

-- | [whitespace] as in spec
pWhitespace :: Parser Text
pWhitespace = takeWhile1 isWhitespace

pSpaces :: Parser Text
pSpaces = takeWhile (== ' ')

pSpacesUpToColumn :: Int -> Parser Text
pSpacesUpToColumn col = do
  currentCol <- column <$> getPosition
  let distance = col - currentCol
  if distance >= 1
    then satisfyUpTo distance (== ' ')
    else pure ""

pIndentSpaces :: Parser Text
pIndentSpaces = do
  nonIndentSpaces <- pNonIndentSpaces
  let count0 = Text.length nonIndentSpaces
  (count1, moreSpace) <- ((4,) <$> char '\t')
                     <|> ((1,) <$> char ' ')
  if count0 + count1 < 4
    then mzero
    else pure $ Text.snoc nonIndentSpaces moreSpace

-- Scan 0-3 spaces.
pNonIndentSpaces :: Parser Text
pNonIndentSpaces = satisfyUpTo 3 (== ' ')

pBlankline :: Parser Text
pBlankline = pSpaces <* endOfInput

-- | [unicode whitespace] as in spec
isUnicodeWhitespace :: Char -> Bool
isUnicodeWhitespace = isSpace

isAsciiPunctuation :: Char -> Bool
isAsciiPunctuation = inClass "!\"#$%&'()*+,./:;<=>?@[\\]^_`{|}~-"

satisfyUpTo :: Int -> (Char -> Bool) -> Parser Text
satisfyUpTo cnt f =
  scan 0 $ \n c ->
    n + 1 <$ guard (n < cnt && f c)

lookupLinkReference
  :: ReferenceMap
  -> Text                -- reference label
  -> Maybe (Text, Maybe Text)  -- (url, title)
lookupLinkReference refmap key =
  Map.lookup (normalizeReference key) refmap

normalizeReference :: Text -> Text
normalizeReference = Text.toCaseFold . Text.concat . Text.split isSpace

parenthesize :: Text -> Text
parenthesize x = "(" <> x <> ")"
