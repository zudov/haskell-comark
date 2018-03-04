{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE ViewPatterns    #-}
module Comark.ParserCombinators.Prim
  ( Position(..)
  , Parser()
  , runParser
  , ParserState()
  , ParseError(..)
  , withConsumed
  , consumedBy
  , string
  , (<?>)
  , runParserWithUnconsumed
  , getPosition
  , setPosition
  , satisfy
  , peekChar
  , peekLastChar
  , replacing
  , endOfInput
  , takeWhile
  , takeWhile1
  , untilTheEnd
  , skip
  , skipWhile
  , skipWhile1
  , stringCaseless
  , scan
  , lookAhead
  , notFollowedBy
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.String
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Prelude             hiding (takeWhile)

data Position
  = Position
      { line   :: {-# UNPACK #-} !Int
      , column :: {-# UNPACK #-} !Int
      , point  :: {-# UNPACK #-} !Int
      } deriving (Ord, Eq)

instance Show Position where
  show (Position ln cn pn) =
    concat ["line ", show ln, " column ", show cn, " point ", show pn]

data ParseError
  = ParseError
      { parseErrorPosition :: Position
      , parseErrorReason   :: String
      } deriving (Show)

data ParserState
  = ParserState
      { subject  :: {-# UNPACK #-} !Text
      , position :: {-# UNPACK #-} !Position
      , lastChar :: !(Maybe Char)
      }

advance :: ParserState -> Text -> ParserState
advance = Text.foldl' go
  where
    go :: ParserState -> Char -> ParserState
    go st c =
      ParserState
        { subject = Text.drop 1 (subject st)
        , position =
            case c of
              '\n' -> Position
                { line   = line  (position st) + 1
                , column = 1
                , point  = point (position st) + 1
                }
              _ -> Position
                { line   = line   (position st)
                , column = column (position st) + 1
                , point  = point  (position st) + 1
                }
        , lastChar = Just c
        }

newtype Parser a
  = Parser
      { evalParser :: ParserState -> Either ParseError (ParserState, a) }

-- | Returns the text that was consumed by a parser alongside with its result
withConsumed :: Parser a -> Parser (a,Text)
withConsumed p = Parser $ \st ->
    case (evalParser p) st of
        Left err -> Left err
        Right (st', res) ->
            let consumedLength = point (position st') - point (position st)
            in Right (st', (res, Text.take consumedLength (subject st)))

consumedBy :: Parser a -> Parser Text
consumedBy = fmap snd . withConsumed

instance Functor Parser where
  fmap f (Parser g) = Parser $ \st ->
    case g st of
         Right (st', x) -> Right (st', f x)
         Left e         -> Left e
  {-# INLINE fmap #-}

instance Applicative Parser where
  pure x = Parser $ \st -> Right (st, x)
  (Parser f) <*> (Parser g) = Parser $ \st ->
    case f st of
         Left e         -> Left e
         Right (st', h) -> case g st' of
                                Right (st'', x) -> Right (st'', h x)
                                Left e          -> Left e
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Alternative Parser where
  empty = Parser $ \st -> Left $ ParseError (position st) "(empty)"
  (Parser f) <|> (Parser g) = Parser $ \st ->
    case f st of
         Right res                 -> Right res
         Left (ParseError pos msg) ->
           case g st of
             Right res                   -> Right res
             Left (ParseError pos' msg') -> Left $
               case () of
                  -- return error for farthest match
                  _ | pos' > pos  -> ParseError pos' msg'
                    | pos' < pos  -> ParseError pos msg
                    | otherwise {- pos' == pos -}
                                  -> ParseError pos (msg ++ " or " ++ msg')
  {-# INLINE empty #-}
  {-# INLINE (<|>) #-}

instance Monad Parser where
  return x = Parser $ \st -> Right (st, x)
  fail e = Parser $ \st -> Left $ ParseError (position st) e
  p >>= g = Parser $ \st ->
    case evalParser p st of
         Left e        -> Left e
         Right (st',x) -> evalParser (g x) st'
  {-# INLINE return #-}
  {-# INLINE (>>=) #-}

instance MonadPlus Parser where
  mzero = Parser $ \st -> Left $ ParseError (position st) "(mzero)"
  mplus p1 p2 = Parser $ \st ->
    case evalParser p1 st of
         Right res -> Right res
         Left _    -> evalParser p2 st
  {-# INLINE mzero #-}
  {-# INLINE mplus #-}

instance (a ~ Text) => IsString (Parser a) where
  fromString = string . Text.pack

string :: Text -> Parser Text
string s = Parser $ \st ->
  if s `Text.isPrefixOf` (subject st)
     then success (advance st s) s
     else failure st "string"
{-# INLINE string #-}

failure :: ParserState -> String -> Either ParseError (ParserState, a)
failure st msg = Left $ ParseError (position st) msg
{-# INLINE failure #-}

success :: ParserState -> a -> Either ParseError (ParserState, a)
success st x = Right (st, x)
{-# INLINE success #-}

(<?>) :: Parser a -> String -> Parser a
p <?> msg = Parser $ \st ->
  let startpos = position st in
  case evalParser p st of
       Left (ParseError _ _) ->
           Left $ ParseError startpos msg
       Right r                 -> Right r
{-# INLINE (<?>) #-}
infixl 5 <?>

runParser :: Parser a -> Text -> Either ParseError a
runParser p t =
  fmap snd $ evalParser p ParserState { subject  = t
                                      , position = Position 1 1 1
                                      , lastChar = Nothing
                                      }

runParserWithUnconsumed :: Parser a -> Text -> Either ParseError (a,Text)
runParserWithUnconsumed p t =
  fmap (\(st,res) -> (res, subject st))
    $ evalParser p ParserState { subject = t
                               , position = Position 1 1 1
                               , lastChar = Nothing
                               }

getState :: Parser ParserState
getState = Parser (\st -> success st st)

getPosition :: Parser Position
getPosition = position <$> getState
{-# INLINE getPosition #-}

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser g
  where g st = case Text.uncons (subject st) of
                    Just (c, _) | f c ->
                         success (advance st (Text.singleton c)) c
                    _ -> failure st "character meeting condition"
{-# INLINE satisfy #-}

-- | Get the next character without consuming it.
peekChar :: Parser (Maybe Char)
peekChar = maybeHead . subject <$> getState
  where maybeHead = fmap fst . Text.uncons
{-# INLINE peekChar #-}

-- | Get the last consumed character.
peekLastChar :: Parser (Maybe Char)
peekLastChar = lastChar <$> getState
{-# INLINE peekLastChar #-}

-- | Takes a parser that returns a 'Text', runs that
--   parser consuming some input and then prepends the
--   result to the rest of subject.
replacing :: Parser Text -> Parser ()
replacing p = do
  s0 <- getState
  t <- p
  s1Subject <- subject <$> getState
  Parser $ \_ -> success s0 { subject = Text.append t s1Subject } ()

endOfInput :: Parser ()
endOfInput = Parser $ \st ->
  if Text.null (subject st)
     then success st ()
     else failure st "end of input"
{-# INLINE endOfInput #-}

-- note: this does not actually change the position in the subject;
-- it only changes what column counts as column N.  It is intended
-- to be used in cases where we're parsing a partial line but need to
-- have accurate column information.
setPosition :: Position -> Parser ()
setPosition pos = Parser $ \st -> success st{ position = pos } ()
{-# INLINE setPosition #-}

takeWhile :: (Char -> Bool) -> Parser Text
takeWhile f = Parser $ \st ->
  let t = Text.takeWhile f (subject st) in
  success (advance st t) t
{-# INLINE takeWhile #-}

takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 f = Parser $ \st ->
  case Text.takeWhile f (subject st) of
       t | Text.null t  -> failure st "characters satisfying condition"
         | otherwise -> success (advance st t) t
{-# INLINE takeWhile1 #-}

-- | Consumes all the available input (until endOfInput) and returns it.
untilTheEnd :: Parser Text
untilTheEnd = Parser $ \st ->
  success (advance st (subject st)) (subject st)
{-# INLINE untilTheEnd #-}

skip :: (Char -> Bool) -> Parser ()
skip f = Parser $ \st ->
  case Text.uncons (subject st) of
       Just (c,_) | f c -> success (advance st (Text.singleton c)) ()
       _          -> failure st "character satisfying condition"
{-# INLINE skip #-}


skipWhile :: (Char -> Bool) -> Parser ()
skipWhile f = Parser $ \st ->
  let t' = Text.takeWhile f (subject st) in
  success (advance st t') ()
{-# INLINE skipWhile #-}

skipWhile1 :: (Char -> Bool) -> Parser ()
skipWhile1 f = Parser $ \st ->
  case Text.takeWhile f (subject st) of
       t' | Text.null t' -> failure st "characters satisfying condition"
          | otherwise -> success (advance st t') ()
{-# INLINE skipWhile1 #-}

stringCaseless :: Text -> Parser Text
stringCaseless (Text.toCaseFold -> s) = Parser $ \st ->
  if Text.toCaseFold s `Text.isPrefixOf` Text.toCaseFold (subject st)
     then success (advance st s) s
     else failure st "stringCaseless"

{-# INLINE stringCaseless #-}

scan :: s -> (s -> Char -> Maybe s) -> Parser Text
scan s0 f = Parser $ go s0 []
  where go s cs st =
         case Text.uncons (subject st) of
               Nothing        -> finish st cs
               Just (c, _)    -> case f s c of
                                  Just s' -> go s' (c:cs)
                                              (advance st (Text.singleton c))
                                  Nothing -> finish st cs
        finish st cs =
            success st (Text.pack (reverse cs))
{-# INLINE scan #-}

lookAhead :: Parser a -> Parser a
lookAhead p = Parser $ \st ->
  either
    (const (failure st "lookAhead"))
    (success st . snd)
    (evalParser p st)
{-# INLINE lookAhead #-}

notFollowedBy :: Parser a -> Parser ()
notFollowedBy p = Parser $ \st ->
  either
    (const (success st ()))
    (const (failure st "notFollowedBy"))
    (evalParser p st)
{-# INLINE notFollowedBy #-}

