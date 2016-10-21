{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module Text.Commonmark.ParserCombinators (
    Position(..)
  , Parser
  , parse
  , parseWithUnconsumed
  , (<?>)
  , satisfy
  , withConsumed
  , consumedBy
  , peekChar
  , peekLastChar
  , notAfter
  , inClass
  , notInClass
  , endOfInput
  , char
  , anyChar
  , getPosition
  , setPosition
  , takeWhile
  , takeTill
  , takeWhile1
  , takeText
  , skip
  , skipWhile
  , skipWhile1
  , replacing
  , string
  , stringCaseless
  , scan
  , lookAhead
  , notFollowedBy
  , option
  , foldP
  , manyTill
  , someTill
  , sepBy1
  , sepEndBy1
  , sepStartEndBy1
  , skipMany
  , skipMany1
  , discardOpt
  , decimal
  , hexadecimal
  ) where
import           Control.Applicative
import           Control.Monad
import           Data.Bits           (Bits, shiftL, (.|.))
import qualified Data.Char           as Char
import qualified Data.Set            as Set
import           Data.String
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Prelude             hiding (takeWhile)

import Text.Commonmark.ParserCombinators.Prim

notAfter :: (Char -> Bool) -> Parser ()
notAfter f = do
  mbc <- peekLastChar
  case mbc of
       Nothing -> return ()
       Just c  -> if f c then mzero else return ()

-- low-grade version of attoparsec's:
charClass :: String -> Set.Set Char
charClass = Set.fromList . go
    where go (a:'-':b:xs) = [a..b] ++ go xs
          go (x:xs) = x : go xs
          go _ = ""
{-# INLINE charClass #-}

inClass :: String -> Char -> Bool
inClass s c = c `Set.member` s'
  where s' = charClass s
{-# INLINE inClass #-}

notInClass :: String -> Char -> Bool
notInClass s = not . inClass s
{-# INLINE notInClass #-}

char :: Char -> Parser Char
char c = satisfy (== c)
{-# INLINE char #-}

anyChar :: Parser Char
anyChar = satisfy (const True)
{-# INLINE anyChar #-}

takeTill f = takeWhile (not . f)
{-# INLINE takeTill #-}

-- | A folding parser
foldP :: (b -> a -> Parser (Maybe b))
      -> Parser a -- ^ A parser that supplies more input
      -> b -- ^ Initial value
      -> Parser b
foldP f p b0 = p >>= go b0
  where go b1 a = f b1 a >>= \case Nothing -> pure b1
                                   Just b2 -> p >>= go b2

{-# INLINE foldP #-}

-- combinators (most definitions borrowed from attoparsec)

option :: Alternative f => a -> f a -> f a
option x p = p <|> pure x
{-# INLINE option #-}

discardOpt :: Alternative f => f a -> f ()
discardOpt p = option () (void p)

someTill :: Alternative f => f a -> f b -> f [a]
someTill p end = liftA2 (:) p go
  where go = (end *> pure []) <|> liftA2 (:) p go
{-# INLINE someTill #-}

manyTill :: Alternative f => f a -> f b -> f [a]
manyTill p end = go
  where go = (end *> pure []) <|> liftA2 (:) p go
{-# INLINE manyTill #-}

sepBy1 :: Alternative f => f a -> f s -> f [a]
sepBy1 p s = go
    where go = liftA2 (:) p ((s *> go) <|> pure [])

sepEndBy1 :: Alternative f => f a -> f s -> f [a]
sepEndBy1 p s = sepBy1 p s <* s

sepStartEndBy1 :: Alternative f => f a -> f s -> f [a]
sepStartEndBy1 p s = s *> sepBy1 p s <* s

skipMany :: Alternative f => f a -> f ()
skipMany p = go
  where go = (p *> go) <|> pure ()
{-# INLINE skipMany #-}

skipMany1 :: Alternative f => f a -> f ()
skipMany1 p = p *> skipMany p
{-# INLINE skipMany1 #-}

-- | Parse and decode an unsigned decimal number.
decimal :: Integral a => Parser a
decimal = T.foldl' step 0 `fmap` takeWhile1 Char.isDigit
  where step a c = a * 10 + fromIntegral (Char.ord c - 48)

hexadecimal :: (Integral a, Bits a) => Parser a
hexadecimal = T.foldl' step 0 `fmap` takeWhile1 isHexDigit
  where
    isHexDigit c = (c >= '0' && c <= '9') ||
                   (c >= 'a' && c <= 'f') ||
                   (c >= 'A' && c <= 'F')
    step a c | w >= 48 && w <= 57  = (a `shiftL` 4) .|. fromIntegral (w - 48)
             | w >= 97             = (a `shiftL` 4) .|. fromIntegral (w - 87)
             | otherwise           = (a `shiftL` 4) .|. fromIntegral (w - 55)
      where w = Char.ord c
