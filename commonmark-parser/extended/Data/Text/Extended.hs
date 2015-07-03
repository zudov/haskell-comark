{-# LANGUAGE OverloadedStrings #-}
module Data.Text.Extended
    ( module Data.Text
    , tabFilter
    , joinLines
    , lines'
    ) where

import           Data.Text
import qualified Data.Text as T

-- Convert tabs to spaces using specified tab stop.
tabFilter :: Int -> Text -> Text
tabFilter tabstop = T.concat . pad . T.split (== '\t')
  where pad []  = []
        pad [t] = [t]
        pad (t:ts) = T.justifyLeft n ' ' t : pad ts
            where tl = T.length t
                  n  = tl + tabstop - (tl `mod` tabstop)

-- | Like unlines but does not add a final newline.
-- | Concatenates lines with newlines between.
joinLines :: [Text] -> Text
joinLines = T.intercalate "\n"

-- | Been commented out from Data.Text
-- /O(n)/ Portably breaks a 'Text' up into a list of 'Text's at line
-- boundaries.
--
-- A line boundary is considered to be either a line feed, a carriage
-- return immediately followed by a line feed, or a carriage return.
-- This accounts for both Unix and Windows line ending conventions,
-- and for the old convention used on Mac OS 9 and earlier.

lines' :: Text -> [Text]
lines' ps | T.null ps = []
          | otherwise = h : case T.uncons t of
                              Nothing -> []
                              Just (c,t')
                                  | c == '\n' -> lines' t'
                                  | c == '\r' -> case T.uncons t' of
                                                   Just ('\n',t'') -> lines' t''
                                                   _               -> lines' t'
                              _ -> error "lines': IMPOSSIBLE HAPPENED"
    where (h,t)    = T.span notEOL ps
          notEOL c = c /= '\n' && c /= '\r'
