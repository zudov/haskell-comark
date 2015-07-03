module Data.Char.Extended
    ( module Data.Char
    , chrSafe
    ) where

import           Data.Char
import           Data.Ix   (inRange)

-- | Invalid Unicode codepoints and NUL will be written as the "unknown codepoint"
--   character (0xFFFD)
chrSafe :: Int -> Char
chrSafe codepoint
    | inRange (0x1,0x10FFFF) codepoint = chr codepoint
    | otherwise = '\xFFFD'
