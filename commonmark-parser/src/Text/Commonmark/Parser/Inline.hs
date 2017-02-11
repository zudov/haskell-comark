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
import           Data.List              (foldl')
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence
  (ViewL(..), singleton, viewl, (<|), (|>))
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
        canOpen =
          isLeft && (isAsterisk indicator || not isRight || precededByPunctuation)
        canClose =
          isRight && (isAsterisk indicator || not isLeft || followedByPunctuation)

    pure $ EmphDelimToken $ EmphDelim indicator (Text.length delim) canOpen canClose
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
  pure $ LinkOpenToken $ LinkOpen openerType True lbl mempty

pEmphLinkDelim :: Parser Token
pEmphLinkDelim = asum
  [ pEmphDelimToken AsteriskIndicator
  , pEmphDelimToken UnderscoreIndicator
  , pLinkOpener
  ]

pEmphTokens :: ParserOptions -> Parser DelimStack
pEmphTokens opts = do
    delim <- pEmphLinkDelim
    foldP
      (\ds -> (Just <$> step ds) <|> (Nothing <$ endOfInput))
      (Seq.singleton delim)
  where
    step ds = asum
      [ char ']' *> lookForLinkOrImage ds
      , (ds |>) <$> pEmphLinkDelim
      , addInlines ds
          <$> pInline opts { poParseEmphasis = False }
      ]

    lookForLinkOrImage :: DelimStack -> Parser DelimStack
    lookForLinkOrImage ds =
      case Seq.findr isLinkOpener ds of
        Nothing -> pure (addInline ds closer)
        Just (suffix, LinkOpenToken opener, prefix) ->
          option fallback $ do
            guard (linkActive opener)
            addInlines (deactivating prefix) <$> pLink
          where
            fallback = addInlines prefix (unLinkOpen opener)
                         <> addInline suffix closer
            constr = case linkOpenerType opener of
                       LinkOpener  -> Link
                       ImageOpener -> Image
            deactivating = case linkOpenerType opener of
                             LinkOpener -> fmap deactivate
                             ImageOpener -> id
            content = foldMap unToken $ processEmphTokens (InlineToken (linkContent opener) <| suffix)
            pLink = pInlineLink constr content
                <|> pReferenceLink constr content (linkLabel opener)
        Just (_, _, _) ->
          error "lookForLinkOrImage: impossible happened. expected LinkOpenToken"

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

processEmphTokens :: DelimStack -> DelimStack
processEmphTokens = foldl' processEmphToken Seq.empty

processEmphToken :: DelimStack -> Token -> DelimStack
processEmphToken stack token =
  case token of
    InlineToken{} ->
      stack |> token
    LinkOpenToken{} ->
      processEmphToken stack (InlineToken $ unToken token)
    EmphDelimToken closing
      | emphCanOpen closing && not (emphCanClose closing) ->
          stack |> EmphDelimToken closing
      | emphCanClose closing ->
      case Seq.findr (matchOpening (emphIndicator closing)) stack of
        Nothing
          | emphCanClose closing ->
              stack |> EmphDelimToken closing
          | otherwise ->
              stack |> InlineToken (unEmphDelim closing)
        Just (viewl -> EmptyL, _, _) -> stack
        Just (content, EmphDelimToken opening, rest)
          | emphCanOpen closing && ((emphLength opening + emphLength closing) `mod` 3) == 0 ->
              stack |> EmphDelimToken closing
          | otherwise ->
              matchEmphStrings rest opening closing
                (foldMap unToken content)
        Just (_, _, _) ->
          error "processEmphToken: Impossible happened. Expected EmphDelimToken"
      | otherwise ->
         stack |> InlineToken (unEmphDelim closing)
  where
    matchOpening ch (EmphDelimToken d) = emphIndicator d == ch && emphCanOpen d
    matchOpening _ _ = False

matchEmphStrings :: DelimStack -> EmphDelim -> EmphDelim -> Inlines Text -> DelimStack
matchEmphStrings stack opening closing content
  | emphIndicator opening == emphIndicator closing = if
     | emphLength closing == emphLength opening ->
         stack
           |> InlineToken (emph (emphLength closing) content)
     | emphLength closing < emphLength opening ->
         stack
           |> EmphDelimToken opening
                { emphLength = emphLength opening - emphLength closing }

           |> InlineToken (emph (emphLength closing) content)
     | emphLength closing > emphLength opening ->
          processEmphToken
            (stack |> InlineToken (emph (emphLength opening) content))
            (EmphDelimToken closing
               { emphLength = emphLength closing - emphLength opening})
     | otherwise -> stack
  | otherwise = stack

emph :: Int -> Inlines Text -> Inlines Text
emph n content
  | n <= 0    = content
  | even n    = single Strong $ emph (n - 2) content
  | otherwise = single Emph   $ emph (n - 1) content
  where
   single f is =
     f is <$ guard (not $ Seq.null is)

pLinkLabel :: Parser Text
pLinkLabel = char '[' *> (Text.concat <$> someTill chunk (char ']'))
  where
    chunk          = regChunk <|> bracketChunk <|> backslashChunk
    regChunk       = takeWhile1 (`notElem` ("[]\\" :: [Char]))
    bracketChunk   = char '\\' *> ("[" <|> "]")
    backslashChunk = "\\\\"

pLinkDest :: Parser Text
pLinkDest =
  pointy <|> nonPointy
  where
    pointy = char '<' *> (Text.concat <$> many chunk) <* char '>'
      where
        chunk = asum
          [ takeWhile1 (`notElem` [' ', '\r', '\n', '>', '<', '\\'])
          , Text.singleton <$> pSatisfy (`notElem` [' ', '\r', '\n', '>', '<'])
          ]
    nonPointy = Text.concat <$> some chunk
      where
        chunk = asum
          [ takeWhile1 (notInClass " ()\\&" <&&> not . isControl)
          , pEntityText
          , pBackslashedChar
          , parenthesize . Text.concat <$> do
              char '('
              manyTill chunk (char ')')
          ]

pLinkTitle :: Parser Text
pLinkTitle = surroundedWith ("('\"'" :: [Char])
  where
    surroundedWith openers = do
      opener <- satisfy (`elem` openers)
      let ender = if opener == '(' then ')' else opener
          pEnder = char ender <* notFollowedBy (skip isAlphaNum)
          regChunk = asum
            [ takeWhile1 ((/= ender) <&&> (/= '\\') <&&> (/= '&'))
            , pEntityText
            , "&"
            , pBackslashedChar
            ]
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
  title <-
    if titleOnNewLine
    then optional $
           pLinkTitle <* do
             skipWhile whitespaceNoNL
             endOfInput <|> void pLineEnding
    else optional pLinkTitle <* do
           skipWhile whitespaceNoNL
           endOfInput <|> void pLineEnding
  pure (lab, url, title)
  where
    -- | optional scanWhitespace (including up to one line ending)
    scanWhitespaceNL = do
      skipWhile whitespaceNoNL
      optional pLineEnding
      skipWhile whitespaceNoNL
    whitespaceNoNL =
      isWhitespace <&&> not . isLineEnding

