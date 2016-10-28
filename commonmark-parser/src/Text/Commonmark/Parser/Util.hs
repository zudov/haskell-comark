{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Text.Commonmark.Parser.Util
  ( pLineEnding
  , isLineEnding
  , isWhitespace
  , parenthesize
  , pNonIndentSpaces
  , isUnicodeWhitespace
  , isAsciiPunctuation
  , Scanner ()
  , pIndentSpaces
  , pBlankline
  , pWhitespace
  , pSpaces
  , satisfyUpTo
  , lookupLinkReference
  , normalizeReference
  , pSpacesUpToColumn
  ) where

import           Control.Applicative
import           Control.Bool
import           Control.Monad
import           Data.Char
import qualified Data.Map                          as M
import           Data.Monoid
import           Data.Text                         (Text)
import qualified Data.Text.Extended                as T
import           Prelude                           hiding (takeWhile)

import           Text.Commonmark.ParserCombinators
import           Text.Commonmark.Types

type Scanner = Parser ()

-- | A newline (U+000A), carriage return (U+000D), or carriage return followed by newline.
pLineEnding :: Parser Text
pLineEnding = "\n" <|> "\r\n" <|> "\r"

-- | Predicate for line ending character (newline or carriage return).
--   NB: something like `satisfy isLineEnding` won't properly parse a
--   line ending, when given '\r\n' as input it would just consume '\r'
--   leaving '\n' unconsumed. In such cases one should use 'pLineEnding'
--   instead
isLineEnding :: Char -> Bool
isLineEnding = (== '\r') <||> (== '\n')

isAsciiPunctuation :: Char -> Bool
isAsciiPunctuation = inClass "!\"#$%&'()*+,./:;<=>?@[\\]^_`{|}~-"

pIndentSpaces :: Parser Text
pIndentSpaces = do
  nonIndentSpaces <- pNonIndentSpaces
  let count0 = T.length nonIndentSpaces
  (count1, moreSpace) <- ((4,) <$> char '\t')
                     <|> ((1,) <$> char ' ')
  if count0 + count1 < 4
    then mzero
    else pure $ T.snoc nonIndentSpaces moreSpace
    
pSpacesUpToColumn :: Int -> Parser Text
pSpacesUpToColumn col = do
  currentCol <- column <$> getPosition
  let distance = col - currentCol
  if distance >= 1
    then satisfyUpTo distance (== ' ')
    else pure ""

-- Scan 0-3 spaces.
pNonIndentSpaces :: Parser Text
pNonIndentSpaces = satisfyUpTo 3 (== ' ')

satisfyUpTo :: Int -> (Char -> Bool) -> Parser Text
satisfyUpTo cnt f =
  scan 0 $ \n c ->
    n + 1 <$ guard (n < cnt && f c)

pBlankline :: Parser Text
pBlankline = pSpaces <* endOfInput

pSpaces :: Parser Text
pSpaces = takeWhile (== ' ')

-- | A [whitespace character] as in spec
isWhitespace :: Char -> Bool
isWhitespace = (`elem` (" \t\n\r\f\v" :: [Char]))

-- | [whitespace] as in spec
pWhitespace :: Parser Text
pWhitespace = takeWhile1 isWhitespace

-- | [unicode whitespace] as in spec
isUnicodeWhitespace :: Char -> Bool
isUnicodeWhitespace = isSpace

normalizeReference :: Text -> Text
normalizeReference = T.toCaseFold . T.concat . T.split isSpace

lookupLinkReference
  :: ReferenceMap
  -> Text                -- reference label
  -> Maybe (Text, Maybe Text)  -- (url, title)
lookupLinkReference refmap key =
  M.lookup (normalizeReference key) refmap

parenthesize :: Text -> Text
parenthesize x = "(" <> x <> ")"
