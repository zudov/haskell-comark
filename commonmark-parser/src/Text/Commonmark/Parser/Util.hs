{-# LANGUAGE OverloadedStrings #-}
module Text.Commonmark.Parser.Util
  ( pLineEnding
  , isLineEnding
  , skipWhitespaceNoNL
  , scanWhitespaceNL
  , isWhitespace
  , parenthesize
  , skipWhitespace
  , isUnicodeWhitespace
  , isAsciiPunctuation
  , Scanner ()
  , scanChar
  , countSpaces
  , scanIndentSpace
  , scanBlankline
  , pWhitespace
  , scanSpaces
  , scanNonindentSpace
  , upToCountChars
  , lookupLinkReference
  , scanSpacesToColumn
  , normalizeReference
  , scanSpacesUpToColumn
  , scanChars
  , countNonindentSpace
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

-- Scan a specified character.
scanChar :: Char -> Scanner
scanChar = skip . (==)

-- Scan a specified character.
scanChars :: Char -> Scanner
scanChars = skipMany . scanChar

-- Scan tab or four spaces.
scanIndentSpace :: Scanner
scanIndentSpace = do
  spaceCount <- countNonindentSpace
  moreSpaceCount <- 4 <$ char '\t'
                <|> 1 <$ char ' '
  when (spaceCount + moreSpaceCount < 4) mzero

scanSpacesToColumn :: Int -> Scanner
scanSpacesToColumn col = do
  currentCol <- column <$> getPosition
  let distance = col - currentCol
  when (distance >= 1)
    $ replicateM_ distance (scanChar ' ')

scanSpacesUpToColumn :: Int -> Scanner
scanSpacesUpToColumn col = do
  currentCol <- column <$> getPosition
  let distance = col - currentCol
  when (distance >= 1)
    $ replicateM_ distance (discardOpt $ scanChar ' ')

-- Scan 0-3 spaces.
scanNonindentSpace :: Scanner
scanNonindentSpace = () <$ upToCountChars 3 (==' ')

countNonindentSpace :: Parser Int
countNonindentSpace = T.length <$> upToCountChars 3 (== ' ')

upToCountChars :: Int -> (Char -> Bool) -> Parser Text
upToCountChars cnt f =
  scan 0 $ \n c ->
    n + 1 <$ guard (n < cnt && f c)

-- Scan a blankline.
scanBlankline :: Scanner
scanBlankline = scanSpaces *> endOfInput

-- Scan 0 or more spaces
scanSpaces :: Scanner
scanSpaces = skipWhile (==' ')

countSpaces :: Parser Int
countSpaces = T.length <$> takeWhile (== ' ')

-- | A [whitespace character] as in spec
isWhitespace :: Char -> Bool
isWhitespace = (`elem` (" \t\n\r\f\v" :: [Char]))

-- | [whitespace] as in spec
pWhitespace :: Parser Text
pWhitespace = takeWhile1 isWhitespace

skipWhitespace :: Scanner
skipWhitespace = skipWhile1 isWhitespace

-- | optional scanWhitespace (including up to one line ending)
scanWhitespaceNL :: Scanner
scanWhitespaceNL = do
 discardOpt scanWhitespaceNoNL
 discardOpt pLineEnding
 discardOpt scanWhitespaceNoNL

scanWhitespaceNoNL :: Scanner
scanWhitespaceNoNL =
  skipWhile1
    (isWhitespace <&&> not . isLineEnding)

skipWhitespaceNoNL :: Scanner
skipWhitespaceNoNL =
  skipWhile
    (isWhitespace <&&> not . isLineEnding)

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
