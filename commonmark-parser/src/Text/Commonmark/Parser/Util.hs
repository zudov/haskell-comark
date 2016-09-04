{-# LANGUAGE OverloadedStrings #-}
module Text.Commonmark.Parser.Util where

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

-- | A newline (U+000A), carriage return (U+000D), or carriage return + newline.
lineEnding :: Scanner
lineEnding = void ("\n" <|> "\r\n" <|> "\r")

nfb :: Parser a -> Parser ()
nfb = notFollowedBy

nfbChar :: Char -> Scanner
nfbChar c = nfb (skip (== c))

isAsciiPunctuation :: Char -> Bool
isAsciiPunctuation = inClass "!\"#$%&'()*+,./:;<=>?@[\\]^_`{|}~-"

-- Scan a specified character.
scanChar :: Char -> Scanner
scanChar c = skip (== c)

-- Scan a specified character.
scanChars :: Char -> Scanner
scanChars c = skipMany (scanChar c)

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
  scan 0 (\n c -> if n < cnt && f c then Just (n+1) else Nothing)

-- Scan a blankline.
scanBlankline :: Scanner
scanBlankline = scanSpaces *> endOfInput

-- Scan 0 or more spaces
scanSpaces :: Scanner
scanSpaces = skipWhile (==' ')

countSpaces :: Parser Int
countSpaces = T.length <$> takeWhile (== ' ')

-- Scan 0 or more spaces, and optionally a newline
-- and more spaces.
scanSpnl :: Scanner
scanSpnl = scanSpaces *> discardOpt (char '\n' *> scanSpaces)

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
 discardOpt lineEnding
 discardOpt scanWhitespaceNoNL

scanWhitespaceNoNL :: Scanner
scanWhitespaceNoNL = skipWhile1 (isWhitespace <&&> (`notElem` ("\r\n" :: [Char])))

skipWhitespaceNoNL :: Scanner
skipWhitespaceNoNL = skipWhile (isWhitespace <&&> (`notElem` ("\r\n" :: [Char])))

-- | [unicode whitespace] as in spec
isUnicodeWhitespace :: Char -> Bool
isUnicodeWhitespace = isSpace

pUnicodeWhitespace :: Parser Text
pUnicodeWhitespace = takeWhile1 isWhitespace

normalizeReference :: Text -> Text
normalizeReference = T.toCaseFold . T.concat . T.split isSpace

lookupLinkReference :: ReferenceMap
                    -> Text                -- reference label
                    -> Maybe (Text, Maybe Text)  -- (url, title)
lookupLinkReference refmap key = M.lookup (normalizeReference key) refmap

parenthesize :: Text -> Text
parenthesize x = "(" <> x <> ")"

bracketize :: Text -> Text
bracketize x = "[" <> x <> "]"

monoidToMaybe :: (Eq a, Monoid a) => a -> Maybe a
monoidToMaybe a
    | a == mempty = Nothing
    | otherwise = Just a