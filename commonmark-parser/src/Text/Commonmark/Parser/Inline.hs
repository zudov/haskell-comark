{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Text.Commonmark.Parser.Inline
    ( parseInlines
    , pReference
    , parseInfoString
    ) where

import Prelude hiding (takeWhile)

import           Control.Applicative
import           Control.Bool
import           Control.Monad          hiding (mapM_)
import           Data.Char.Extended
import           Data.Foldable          (asum)
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence          (Seq, ViewL (..), singleton, viewl,
                                         (<|), (|>))
import qualified Data.Sequence.Extended as Seq
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Lazy.Builder

import Text.Commonmark.Parser.Inline.EmphLink
import Text.Commonmark.Parser.Options
import Text.Commonmark.Parser.Util
import Text.Commonmark.ParserCombinators
import Text.Commonmark.Syntax
import Text.Commonmark.Syntax.Builder
import Text.Html.Email.Validate
import Text.Html.Entity

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

parseInlines :: ParserOptions -> Text -> Inlines Text
parseInlines opts t = normalization $
        case parse (msum <$> many (pInline opts) <* endOfInput) t of
            Left e -> error ("parseInlines: " ++ show e) -- should not happen
            Right r -> r
    where normalization
            | poNormalize opts = fmap (fmap (Text.Lazy.toStrict . Text.Lazy.Builder.toLazyText))
                                     . normalize
                                     . fmap (fmap Text.Lazy.Builder.fromText)
            | otherwise = id

pInline :: ParserOptions -> Parser (Inlines Text)
pInline opts =  pText
            <|> pHardbreak
            <|> pSoftbreak
            <|> guard (poParseEmphasis opts) *> pEmphLink opts
            <|> pBackslashed
            <|> pAutolink
            <|> pHtml
            <|> pCode
            <|> pEntity
            <|> pFallback

parseInfoString :: Text -> Text
parseInfoString t =
    case parse (msum <$> (many parser <* endOfInput)) t of
        Left _ -> t
        Right is -> foldMap asText is
    where parser = pText <|> pBackslashed <|> pEntity

pText :: Parser (Inlines Text)
pText = str <$> takeWhile1 (not . isSpecial)

pFallback :: Parser (Inlines Text)
pFallback = str <$> (Text.singleton <$> satisfy isSpecial)

isSpecial :: Char -> Bool
isSpecial = inClass "\\`*_[]!&<\t\n\r "

-- | Either backslash-escaped punctuation or an actual backslash
pBackslashedChar :: Parser Text
pBackslashedChar =
    Text.singleton <$> (char '\\' *> option '\\' (satisfy isAsciiPunctuation))

-- Parses a (possibly escaped) character satisfying the predicate.
pSatisfy :: (Char -> Bool) -> Parser Char
pSatisfy p = satisfy ((/= '\\') <&&> p)
          <|> (char '\\' *> satisfy (isAsciiPunctuation <&&> p))
          <|> (guard (p '\\') >> char '\\')

pBackslashed :: Parser (Inlines Text)
pBackslashed = str <$> pBackslashedChar

pHardbreak :: Parser (Inlines Text)
pHardbreak =
  singleton HardBreak
    <$ asum [ void (char '\\'), spaceScape ] <* pLineEnding
    <* skipWhile (== ' ') -- ignore next line's leading spaces
  where
    spaceScape = do
      replicateM 2 (char ' ')  -- two spaces
      skipWhile (== ' ')       -- and more spaces (optionally)

pSoftbreak :: Parser (Inlines Text)
pSoftbreak = discardOpt (char ' ') *> pLineEnding
                                   *> pure (singleton SoftBreak)
                                   <* skipWhile (== ' ')

-- [ Code ] --------------------------------------------------------------------

pCode :: Parser (Inlines Text)
pCode = singleton <$> do
  startTicks <- backtickChunk
  let pEndTicks = string startTicks <* notFollowedBy (char '`')
      pContent  = code <$> (codechunk `manyTill` pEndTicks)
      fallback  = Str startTicks
  pContent <|> pure fallback
  where
    code      = Code . Text.strip . Text.concat
    codechunk = backtickChunk <|> nonBacktickChunk <|> spaceChunk

    backtickChunk      = takeWhile1 (== '`')
    nonBacktickChunk   = takeWhile1 ((/= '`') <&&> (not . isCollapsableSpace))
    spaceChunk         =  " " <$ takeWhile1 isCollapsableSpace
    isCollapsableSpace = (== ' ') <||> isLineEnding

-- [ Raw Html ] ----------------------------------------------------------------
pHtml :: Parser (Inlines Text)
pHtml =
  singleton . RawHtml <$> consumedBy (asum scanners)
  where
    scanners =
      [ void tag, void comment, void instruction, void declaration, void cdata ]
    instruction = "<?" *> manyTill anyChar "?>"
    cdata = "<![CDATA[" *> manyTill anyChar "]]>"
    declaration = do
      "<!" *> skipWhile1 isAsciiUpper
      pWhitespace
      skipWhile (/= '>') <* char '>'
    comment = do
      "<!--" *> notFollowedBy (">" <|> "->")
      comm <- Text.pack <$> manyTill anyChar "-->"
      guard $ not $ or
        [ Text.head comm == '>'
        , "->" `Text.isPrefixOf` comm
        , Text.last comm == '-'
        , "--" `Text.isInfixOf` comm
        ]
    tag = openTag <|> closeTag
      where
        openTag = do
          "<" *> tagName
          many attr <* optional pWhitespace
          optional "/" *> ">"
          where
            attr = do
              pWhitespace *> attrName
              optional attrValueSpec
              where
                attrName = do
                  satisfy ((isAscii <&&> isLetter) <||> inClass "_:")
                  skipWhile ((isAscii <&&> (isLetter <||> isDigit)) <||> inClass "_:.-")
                attrValueSpec = do
                  optional pWhitespace *> char '='
                  optional pWhitespace *> attrValue
                attrValue =
                  unquoted <|> singleQuoted <|> doubleQuoted
                  where
                    unquoted     = skipWhile1 (notInClass " \"'=<>`")
                    singleQuoted = char '\'' *> skipWhile (/= '\'') <* char '\''
                    doubleQuoted = char '"'  *> skipWhile (/= '"')  <* char '"'
        closeTag =
          "</" *> tagName *> optional pWhitespace *> ">"
        tagName = do
          satisfy (isAscii <&&> isLetter)
          skipWhile ((== '-') <||> (isAscii <&&> (isLetter <||> isDigit)))

-- [ Entities ] ----------------------------------------------------------------

pEntity :: Parser (Inlines Text)
pEntity = str <$> pEntityText

pEntityText :: Parser Text
pEntityText =
    char '&' *> entityBody <* char ';'
  where
    entityBody =
      codepointEntity <|> namedEntity
    codepointEntity = do
      char '#'
      decEntity <|> hexEntity
    namedEntity = do
      name <- takeWhile1 (/= ';')
      -- asum works over Maybe. Nothing turns into mzero.
      asum (pure <$> entityNameChars name)
        <?> "not a named entity"
    decEntity =
      Text.singleton . chrSafe <$> decimal
    hexEntity = do
      char 'x' <|> char 'X'
      Text.singleton . chrSafe <$> hexadecimal

-- [ Autolinks ] ---------------------------------------------------------------
pAutolink :: Parser (Inlines Text)
pAutolink = char '<' *> (pUrl <|> pEmail) <* char '>'
    where pUrl = do
              scheme <- pScheme
              _ <- char ':'
              chars <- takeTill ((isAscii <&&> (isWhitespace <||> isControl))
                                    <||> (== '>') <||> (== '<'))
              let uri = scheme <> ":" <> chars
              pure $ singleton $ Link (str uri) uri Nothing
          pEmail = do
              email <- isValidEmail `mfilter` takeWhile1 (/= '>')
              pure $ singleton $
                Link (str email) ("mailto:" <> email) Nothing

pScheme :: Parser Text
pScheme = do
  a <- satisfy (isAscii <&&> isLetter)
  as <- takeWhile1 ((isAscii <&&> (isLetter <||> isDigit)) <||> (== '+') <||> (== '.') <||> (== '-'))
  mfilter ((<= 32) . Text.length) $ pure $ Text.cons a as

-- [ Emphasis, Links, and Images ] ---------------------------------------------

pEmphDelimToken :: EmphIndicator -> Parser Token
pEmphDelimToken indicator@(indicatorChar -> c) = do
    preceded <- peekLastChar
    delim <- takeWhile1 (== c)
    followed <- peekChar
    let isLeft  = check followed preceded
        isRight = check preceded followed
        precededByPunctuation = fromMaybe False (isPunctuation <$> preceded)
        followedByPunctuation = fromMaybe False (isPunctuation <$> followed)
    pure $ case indicator of
      AsteriskIndicator ->
        EmphDelimToken AsteriskIndicator (Text.length delim) isLeft isRight

      UnderscoreIndicator ->
        EmphDelimToken UnderscoreIndicator (Text.length delim)
          (isLeft && (not isRight || precededByPunctuation))
          (isRight && (not isLeft || followedByPunctuation))
    where
        check :: Maybe Char -> Maybe Char -> Bool
        check a b = fromMaybe False (not . isUnicodeWhitespace <$> a) &&
                    fromMaybe True
                       ((not . isPunctuation <$> a) <||>
                       ((isUnicodeWhitespace <||> isPunctuation) <$> b))

pLinkOpener :: Parser Token
pLinkOpener = do
  openerType <- option LinkOpener (ImageOpener <$ char '!')
  lbl <- optional (lookAhead pLinkLabel)
  _ <- char '['
  pure $ LinkOpenToken openerType True lbl mempty

pEmphLinkDelim :: Parser Token
pEmphLinkDelim = asum
  [ pEmphDelimToken AsteriskIndicator
  , pEmphDelimToken UnderscoreIndicator
  , pLinkOpener
  ]

pEmphTokens :: ParserOptions -> Parser (Seq Token)
pEmphTokens opts =
    go . Seq.singleton =<< pEmphLinkDelim
  where
    go :: Seq Token -> Parser (Seq Token)
    go ds = (go =<< step)
        <|> (ds <$ endOfInput)
      where
        step = asum
          [ char ']' *> lookForLinkOrImage ds
          , pEmphLinkDelim <&> (ds |>)
          , pInline opts { poParseEmphasis = False }
              <&> addInlines ds
          ]

    lookForLinkOrImage :: Seq Token -> Parser (Seq Token)
    lookForLinkOrImage ds =
      case Seq.findr isLinkOpener ds of
        Nothing -> pure (addInline ds closer)
        Just (suffix, opener, prefix) ->
          option fallback $ do
            guard (active opener)
            addInlines (deactivating prefix) <$> pLink
          where
            fallback = addInlines prefix (unToken opener)
                         <> addInline suffix closer
            constr = case openerType opener of
                       LinkOpener  -> Link
                       ImageOpener -> Image
            deactivating = case openerType opener of
                             LinkOpener -> fmap deactivate
                             ImageOpener -> id
            linkContent = foldMap unToken $ processEmphTokens (InlineToken (content opener) <| suffix)
            pLink = pInlineLink constr linkContent
                <|> pReferenceLink constr linkContent (refLabel opener)
      where
        closer = Str "]"

    pInlineLink constr content = do
      char '(' *> optional pWhitespace
      dest <- option "" pLinkDest
      title <- optional (pWhitespace *> pLinkTitle <* optional pWhitespace)
      char ')'
      pure $ singleton $ constr content dest title

    pReferenceLink constr content lbl = do
      ref <- (Just <$> pLinkLabel) <|> (lbl <$ optional "[]")
      maybe mzero (pure . singleton . uncurry (constr content))
                  (poLinkReferences opts =<< ref)

pEmphLink :: ParserOptions -> Parser (Inlines Text)
pEmphLink opts =
  foldMap unToken . processEmphTokens <$> pEmphTokens opts

processEmphTokens :: Seq Token -> Seq Token
processEmphTokens = Seq.reverse . foldl (flip processEmphToken) Seq.empty

processEmphToken :: Token -> Seq Token -> Seq Token
processEmphToken closing@EmphDelimToken{} stack
  | dCanOpen closing && not (dCanClose closing) = closing <| stack
  | dCanClose closing =
      case Seq.findl (matchOpening (dChar closing)) stack of
        Nothing
          | dCanOpen closing -> closing <| stack
          | otherwise -> (InlineToken $ unToken closing) <| stack
        Just (viewl -> EmptyL, _, _) -> stack
        Just (content, opening, rest)
          | dCanOpen closing && ((dLength opening + dLength closing) `mod` 3) == 0 ->
              closing <| stack
          | otherwise ->
              matchEmphStrings rest opening closing
                (foldMap unToken $ Seq.reverse content)
  | otherwise = InlineToken (unToken closing) <| stack
  where
    matchOpening ch d = isEmphDelim d && dChar d == ch && dCanOpen d

processEmphToken inline@InlineToken{} stack = inline <| stack
processEmphToken lo@LinkOpenToken{} stack = processEmphToken (InlineToken (unToken lo)) stack

matchEmphStrings :: Seq Token -> Token -> Token -> Inlines Text -> Seq Token
matchEmphStrings stack opening closing content
  | dChar opening == dChar closing = if
     | dLength closing == dLength opening ->
         InlineToken (emph (dLength closing) content) <| stack
     | dLength closing < dLength opening ->
            InlineToken (emph (dLength closing) content)
          <| opening {dLength = dLength opening - dLength closing}
          <| stack
     | dLength closing > dLength opening ->
          processEmphToken (closing { dLength = dLength closing - dLength opening})
                           (InlineToken (emph (dLength opening) content) <| stack)
     | otherwise -> stack
  | otherwise = stack

emph :: Int -> Inlines Text -> Inlines Text
emph 1 content = single Emph content
emph 2 content = single Strong content
emph n content = single Strong $ emph (n - 2) content

-- singleton sequence or empty if contents are empty
single :: (Inlines a -> Inline a) -> Inlines a -> Inlines a
single constructor ils | Seq.null ils = mempty
                       | otherwise    = singleton (constructor ils)

pLinkLabel :: Parser Text
pLinkLabel = char '[' *> (Text.concat <$> someTill chunk (char ']'))
  where chunk = regChunk <|> bracketChunk <|> backslashChunk
        regChunk = takeWhile1 (`notElem` ("[]\\" :: [Char]))
        bracketChunk = char '\\' *> ("[" <|> "]")
        backslashChunk = "\\\\"

pLinkDest :: Parser Text
pLinkDest = do
  inPointy <- (True <$ char '<') <|> pure False
  if inPointy
     then Text.pack <$> manyTill (pSatisfy (`notElem` ("\r\n " :: [Char]))) (char '>')
     else Text.concat <$> some (regChunk <|> parenChunk)
  where regChunk = takeWhile1 (notInClass " \n\r()\\&" <&&> (not . isControl))
                <|> pEntityText <|> pBackslashedChar
        parenChunk = parenthesize . Text.concat <$> (char '(' *>
                       manyTill regChunk (char ')'))

pLinkTitle :: Parser Text
pLinkTitle = surroundedWith ("('\"'" :: [Char])
    where surroundedWith openers = do
              opener <- satisfy (`elem` openers)
              let ender = if opener == '(' then ')' else opener
                  pEnder = char ender <* notFollowedBy (skip isAlphaNum)
                  regChunk = takeWhile1 ((/= ender) <&&> (/= '\\') <&&> (/= '&'))
                          <|> pEntityText <|> "&" <|> pBackslashedChar
                  nestedChunk = parenthesize <$> surroundedWith "("
              Text.concat <$> manyTill (regChunk <|> nestedChunk) pEnder

pReference :: Parser (Text, Text, Maybe Text)
pReference = do
    lab <- pLinkLabel <* char ':'
    guard $ isJust $ Text.find (not . isWhitespace) lab
    scanWhitespaceNL
    url <- pLinkDest <* skipWhile whitespaceNoNL
    titleOnNewLine <- isJust <$> optional pLineEnding
    skipWhile whitespaceNoNL
    title <- if titleOnNewLine
             then optional (pLinkTitle <* skipWhile whitespaceNoNL
                                       <* (endOfInput <|> void pLineEnding))
             else optional pLinkTitle <* skipWhile whitespaceNoNL
                                      <* (endOfInput <|> void pLineEnding)
    pure (lab, url, title)
  where
    -- | optional scanWhitespace (including up to one line ending)
    scanWhitespaceNL = skipWhile whitespaceNoNL
                    *> optional pLineEnding
                    *> skipWhile whitespaceNoNL
    whitespaceNoNL = isWhitespace <&&> not . isLineEnding

