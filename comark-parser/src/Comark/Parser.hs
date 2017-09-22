{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}
module Comark.Parser
  ( parse
  , ParserOption(..)
  ) where

import Prelude hiding (takeWhile)

import           Control.Applicative
import           Control.Arrow                  (second)
import           Control.Bool
import           Control.Monad
import           Control.Monad.Trans.RWS.Strict
import           Data.Char
import           Data.Either
import           Data.Foldable
import           Data.List                      (intercalate)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Maybe                     (mapMaybe)
import           Data.Monoid
import           Data.Sequence
  (Seq, ViewL(..), ViewR(..), singleton, viewl, viewr, (<|), (><), (|>))
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as Set
import           Data.Text.Extended             (Text)
import qualified Data.Text.Extended             as Text

import Comark.Parser.Inline
import Comark.Parser.Options
import Comark.Parser.Reference
import Comark.Parser.Util
import Comark.ParserCombinators
import Comark.Syntax

-- | Parses Commonmark document. Any sequence of characters is a valid
--   Commonmark document.
--
--   At the moment no sanitizations are performed besides the ones defined
--   in the spec.
parse :: [ParserOption] -> Text -> Doc Text
parse (parserOptions -> opts) text =
  Doc $ processDocument
      $ second extendRefmap
      $ processLines text
  where
    extendRefmap refmap =
      opts { _poLinkReferences =
               \t -> Map.lookup (toLinkLabel t) refmap
                      <|> _poLinkReferences opts t
           }

-- General parsing strategy:
--
-- Step 1:  processLines
--
-- We process the input line by line.  Each line modifies the
-- container stack, by adding a leaf to the current open container,
-- sometimes after closing old containers and/or opening new ones.
--
-- To open a container is to add it to the top of the container stack,
-- so that new content will be added under this container.
-- To close a container is to remove it from the container stack and
-- make it a child of the container above it on the container stack.
--
-- When all the input has been processed, we close all open containers
-- except the root (Document) container.  At this point we should also
-- have a ReferenceMap containing any defined link references.
--
-- Step 2:  processDocument
--
-- We then convert this container structure into an AST.  This principally
-- involves (a) gathering consecutive ListItem containers into lists, (b)
-- gathering TextLine nodes that don't belong to verbatim containers into
-- paragraphs, and (c) parsing the inline contents of non-verbatim TextLines.

--------

-- Container stack definitions:
data ContainerStack
  = ContainerStack
      { csTop  :: Container
      , csRest :: [Container]
      }

type LineNumber = Int

data Elt
  = C Container
  | L LineNumber Leaf
  deriving (Show)

data Container =
  Container
    { containerType     :: ContainerType
    , containerChildren :: Seq Elt
    }

data ContainerType
  = Document
  | BlockQuote
  | ListItem
      { liPadding :: Int
      , liType    :: ListType
      }
  | FencedCode
      { codeStartColumn :: Int
      , codeFence       :: Text
      , codeInfo        :: Maybe Text
      }
  | IndentedCode
  | RawHtmlBlock Condition
  | Reference
  deriving (Show, Eq)

isIndentedCode :: Elt -> Bool
isIndentedCode (C (Container IndentedCode _)) = True
isIndentedCode _                              = False

isBlankLine :: Elt -> Bool
isBlankLine (L _ BlankLine{}) = True
isBlankLine _                 = False

isTextLine :: Elt -> Bool
isTextLine (L _ (TextLine _)) = True
isTextLine _                  = False

isListItem :: ContainerType -> Bool
isListItem ListItem{} = True
isListItem _          = False

instance Show Container where
    show c = mconcat
      [ show (containerType c), "\n"
      , nest 2 (intercalate "\n" $ map pptElt $ toList $ containerChildren c)
      ]

nest :: Int -> String -> String
nest num = intercalate "\n" . map (replicate num ' ' <>) . lines

pptElt :: Elt -> String
pptElt (C c)              = show c
pptElt (L _ (TextLine s)) = show s
pptElt (L _ lf)           = show lf

type ReferenceMap = Map LinkLabel (LinkDestination, Maybe LinkTitle)

-- | Scanners that must be satisfied if the current open container is to be
--   continued on a new line (ignoring lazy continuations).
containerContinue :: Container -> Scanner
containerContinue c = case containerType c of
    BlockQuote     -> pNonIndentSpaces *> scanBlockquoteStart
    IndentedCode   -> void pIndentSpaces
    FencedCode{..} -> void $ pSpacesUpToColumn codeStartColumn
    ListItem{..}   -> void pBlankline <|> (tabCrusher *> replicateM_ liPadding (char ' '))
    -- TODO: This is likely to be incorrect behaviour. Check.
    Reference -> notFollowedBy
      (void pBlankline
         <|> (do _ <- pNonIndentSpaces
                 scanReference <|> scanBlockquoteStart <|> scanTBreakLine)
         <|> void parseAtxHeadingStart)
    _ -> pure ()
{-# INLINE containerContinue #-}

-- | Defines parsers that open new containers.
containerStart :: Bool -> Bool -> Parser ContainerType
containerStart afterListItem lastLineIsText = asum
    [ pNonIndentSpaces
      *> scanBlockquoteStart
      *> pure BlockQuote
    , parseListMarker afterListItem lastLineIsText
    ]

-- | Defines parsers that open new verbatim containers (containers
--   that take only TextLine and BlankLine as children).
verbatimContainerStart :: Bool -> Parser ContainerType
verbatimContainerStart lastLineIsText = asum
   [ pNonIndentSpaces *> parseCodeFence
   , do guard (not lastLineIsText)
        void pIndentSpaces
        notFollowedBy pBlankline
        pure IndentedCode
   , RawHtmlBlock <$> pHtmlBlockStart lastLineIsText
   , guard (not lastLineIsText) *> pNonIndentSpaces *> (Reference <$ scanReference)
   ]

-- | Leaves of the container structure (they don't take children).
type Leaf = GenLeaf Text

data GenLeaf t
  = TextLine         t
  | BlankLine        t
  | ATXHeading    HeadingLevel t
  | SetextHeading HeadingLevel t
  | SetextToken   HeadingLevel t
  | Rule
  deriving (Show, Functor)

type ContainerM = RWS () ReferenceMap ContainerStack

-- | Close the whole container stack, leaving only the root Document container.
closeStack :: ContainerM Container
closeStack =
  get >>= \case
    ContainerStack top [] -> pure top
    ContainerStack _ _    -> closeContainer *> closeStack

-- Close the top container on the stack.  If the container is a Reference
-- container, attempt to parse the reference and update the reference map.
-- If it is a list item container, move a final BlankLine outside the list
-- item.
closeContainer :: ContainerM ()
closeContainer =
  get >>= \case
    ContainerStack top@(Container Reference cs'') (Container ct' cs' : rs) ->
      case runParserWithUnconsumed pReference input of
        Right ((lab, lnk, tit), unconsumed) -> do
          tell $ Map.singleton (toLinkLabel lab) (lnk, tit)
          let cs | Text.null unconsumed = rest'
                 | otherwise = L (-1) (TextLine unconsumed) <| rest'
          put $ ContainerStack (Container ct' (cs <> cs' |> C top)) rs
        Left _ ->
          put $ ContainerStack (Container ct' (cs' <> cs'')) rs
      where
        input = Text.strip $ Text.joinLines $ map extractText $ toList textlines
        (textlines, rest') = Seq.spanl isTextLine cs''

    ContainerStack top (Container ct' cs' : rs)
      | Container li@ListItem{} (viewr -> zs :> b) <- top
      , isBlankLine b ->
          let els = if null zs
                    then cs' |> C (Container li zs)
                    else cs' |> C (Container li zs) |> b
          in put $ ContainerStack (Container ct' els) rs

    ContainerStack top (Container ct' cs' : rs) ->
      put $ ContainerStack (Container ct' (cs' |> C top)) rs

    ContainerStack _ [] -> pure ()

-- Add a leaf to the top container.
addLeaf :: LineNumber -> Leaf -> ContainerM ()
addLeaf lineNum lf = do
  ContainerStack top rest <- get
  case containerType top of
    ListItem{} -- two blanks break out of list item:
      | (firstLine :< _) <- viewl $ containerChildren top
      , BlankLine{} <- lf
      , isBlankLine firstLine -> do
          closeContainer
          addLeaf lineNum lf
    _ -> put $ ContainerStack
                 (Container
                   (containerType top)
                   (containerChildren top |> L lineNum lf))
               rest

-- Add a container to the container stack.
addContainer :: ContainerType -> ContainerM ()
addContainer ct =
  modify $ \ContainerStack{..} ->
    ContainerStack (Container ct mempty) (csTop:csRest)

-- Step 2

-- Convert Document container and reference map into an ASText.
processDocument :: (Container, ParserOptions) -> Blocks Text
processDocument (Container Document cs, opts) = processElts opts (toList cs)
processDocument _ = error "top level container is not Document"

-- Turn the result of `processLines` into a proper ASText.
-- This requires grouping text lines into paragraphs
-- and list items into lists, handling blank lines,
-- parsing inline contents of texts and resolving referencess.
processElts :: ParserOptions -> [Elt] -> Blocks Text
processElts _ [] = mempty

processElts opts (L _lineNumber lf : rest) =
  case lf of
    -- Gobble text lines and make them into a Para:
    TextLine t ->
      singleton (Para $ parseInlines opts txt) <> processElts opts rest'
        where txt = Text.stripEnd $ Text.joinLines $ map Text.stripStart
                               $ t : map extractText textlines
              (textlines, rest') = span isTextLine rest

    -- Blanks at outer level are ignored:
    BlankLine{} -> processElts opts rest

    -- Headings:
    ATXHeading lvl t -> Heading lvl (parseInlines opts t) <| processElts opts rest
    SetextHeading lvl t -> Heading lvl (parseInlines opts t) <| processElts opts rest
    SetextToken _ _ -> error "Setext token wasn't converted to setext header"

    -- Horizontal rule:
    Rule -> ThematicBreak <| processElts opts rest

processElts opts (C (Container ct cs) : rest) =
  case ct of
    Document -> error "Document container found inside Document"

    BlockQuote -> Quote (processElts opts (toList cs)) <| processElts opts rest

    -- List item?  Gobble up following list items of the same type
    -- (skipping blank lines), determine whether the list is tight or
    -- loose, and generate a List.
    ListItem { liType = itemType } ->
      List itemType isTight (Seq.fromList items') <| processElts opts rest'
        where
          xs = takeListItems rest

          rest' = drop (length xs) rest

          -- take list items as long as list type matches and we
          -- don't hit two blank lines:
          takeListItems (c@(C (Container ListItem { liType = lt } _)) : zs)
            | listTypesMatch lt itemType = c : takeListItems zs
          takeListItems (lf@(L _ (BlankLine _)) : c@(C (Container ListItem { liType = lt } _)) : zs)
            | listTypesMatch lt itemType = lf : c : takeListItems zs
          takeListItems _ = []

          listTypesMatch (Bullet c1) (Bullet c2)       = c1 == c2
          listTypesMatch (Ordered w1 _) (Ordered w2 _) = w1 == w2
          listTypesMatch _ _                           = False

          items = mapMaybe getItem (Container ct cs : [c | C c <- xs])

          getItem (Container ListItem{} cs') = Just $ toList cs'
          getItem _                          = Nothing

          items' = map (processElts opts) items

          isTight = not (any isBlankLine xs) && all tightListItem items

          tightListItem []     = True
          tightListItem [_]    = True
          tightListItem (_:is) = not $ any isBlankLine $ init is

    FencedCode _ _ info -> CodeBlock (parseInfoString <$> info)
                                     (Text.unlines $ map extractText $ toList cs)
                                <| processElts opts rest

    IndentedCode -> CodeBlock Nothing txt <| processElts opts rest'
        where txt = Text.unlines $ stripTrailingEmpties $ concatMap extractCode cbs
              stripTrailingEmpties = reverse . dropWhile (Text.all (== ' ')) . reverse

              -- explanation for next line:  when we parsed
              -- the blank line, we dropped 0-3 spaces.
              -- but for this, code block context, we want
              -- to have dropped 4 spaces. we simply drop
              -- one more:
              extractCode (L _ (BlankLine t)) = [Text.drop 1 t]
              extractCode (C (Container IndentedCode cs')) =
                  map extractText $ toList cs'
              extractCode _ = []

              (cbs, rest') = span (isIndentedCode <||> isBlankLine)
                                  (C (Container ct cs) : rest)

    RawHtmlBlock _ -> HtmlBlock txt <| processElts opts rest
        where txt = Text.unlines (map extractText (toList cs))

    -- References have already been taken into account in the reference map,
    -- so we just skip.
    Reference -> processElts opts rest

extractText :: Elt -> Text
extractText (L _ (TextLine t)) = t
extractText _                  = mempty

-- Step 1

processLines :: Text -> (Container, ReferenceMap)
processLines t = evalRWS (mapM_ processLine lns >> closeStack) () initState
  where
    lns = zip [1..] $ Text.lines' $ Text.replace "\0" "\xFFFD" t
    initState = ContainerStack (Container Document mempty) []

-- The main block-parsing function.
-- We analyze a line of text and modify the container stack accordingly,
-- adding a new leaf, or closing or opening containers.
processLine :: (LineNumber, Text) -> ContainerM ()
processLine (lineNumber, txt) = do
  ContainerStack top@(Container ct cs) rest <- get
  -- Apply the line-start scanners appropriate for each nested container.
  -- Return the remainder of the string, and the number of unmatched
  -- containers.
  let (t', numUnmatched) = tryOpenContainers (reverse $ top:rest) txt

  -- Some new containers can be started only after a blank.
  let lastLineIsText = numUnmatched == 0 &&
                       case viewr cs of
                            (_ :> L _ (TextLine _)) -> True
                            _                       -> False

  -- Process the rest of the line in a way that makes sense given
  -- the container type at the top of the stack (ct):
  case ct of
    -- If it's a verbatim line container, add the line.
    RawHtmlBlock c
      | numUnmatched == 0 -> do
          addLeaf lineNumber (TextLine t')
          when (isRight $ runParser (blockEnd c) t')
            closeContainer
    IndentedCode
      | numUnmatched == 0 -> addLeaf lineNumber (TextLine t')
    FencedCode { codeFence = fence' }
      | numUnmatched == 0 -> if
           -- closing code fence
        | isRight $ runParser scanClosing t'
            -> closeContainer
        | otherwise
            -> addLeaf lineNumber (TextLine t')
        where
          scanClosing = satisfyUpTo 3 (== ' ')
                     *> string fence' *> skipWhile (== Text.head fence')
                     *> pSpaces
                     *> endOfInput


    -- otherwise, parse the remainder to see if we have new container starts:
    _ -> let (verbatimContainers, leaf) =
               tryNewContainers (isListItem ct) lastLineIsText (Text.length txt - Text.length t') t'
         in case (Seq.viewl verbatimContainers, leaf) of
              -- lazy continuation: text line, last line was text, no new containers,
              -- some unmatched containers:
              (Seq.EmptyL, TextLine t)
                  | numUnmatched > 0
                  , _ :> L _ TextLine{} <- viewr cs
                  , ct /= IndentedCode
                  -> addLeaf lineNumber (TextLine t)

              -- Special case: Lazy continuation of a list item looking like
              -- indented code e.g:
              -- "  1.  Paragraph"
              -- "    with two lines"
              (IndentedCode :< _, TextLine t)
                  | numUnmatched > 0
                  , _ :> L _ TextLine{} <- viewr cs
                  , ListItem{} <- ct
                  -> addLeaf lineNumber $ TextLine $ Text.strip t

              -- A special case: Lazy continuaation of a quote looking like
              -- indented code e.g:
              -- "> foo"
              -- "    - bar"
              (IndentedCode :< _, TextLine t)
                | numUnmatched > 0
                , _ :> L _ TextLine{} <- viewr cs
                , BlockQuote{} <- ct
                -> addLeaf lineNumber $ TextLine $ Text.strip t

              -- if it's a setext header line and the top container has a textline
              -- as last child, add a setext header:
              (Seq.EmptyL, SetextToken lev _setextText) | numUnmatched == 0 ->
                  case Seq.spanr isTextLine cs of
                    (textlines, cs') -- gather all preceding textlines and put them in the header
                      | not (Seq.null textlines)
                      -> put $ ContainerStack
                           (Container ct
                             (cs' |> L lineNumber
                               (SetextHeading
                                  lev
                                  (Text.strip $ Text.unlines
                                           $ fmap extractText
                                           $ toList textlines))))
                           rest
                        -- Note: the following case should not occur, since
                        -- we don't add a SetextHeading leaf unless lastLineIsText.
                      | otherwise -> error "setext header line without preceding text lines"

              -- The end tag can occur on the same line as the start tag.
              (RawHtmlBlock condition :< _, TextLine t)
                | Right () <- runParser (blockEnd condition) t
                -> do closeContainer
                      addContainer (RawHtmlBlock condition)
                      addLeaf lineNumber (TextLine t)
                      closeContainer
              -- otherwise, close all the unmatched containers, add the new
              -- containers, and finally add the new leaf:
              (ns, lf) -> do -- close unmatched containers, add new ones
                  _ <- replicateM numUnmatched closeContainer
                  _ <- mapM_ addContainer ns
                  case (Seq.viewr verbatimContainers, lf) of
                    -- don't add extra blank at beginning of fenced code block
                    (_ :>FencedCode{}, BlankLine{}) -> pure ()
                    _                               -> addLeaf lineNumber lf

tabCrusher :: Parser ()
tabCrusher = do
  p <- getPosition
  replacing (go (column p - 1) "")
  where
    go cnt acc = do
      c <- peekChar
      case c of
        Just ' '  -> char ' '  *> go (cnt + 1) (acc <> " ")
        Just '\t' -> char '\t' *> go (cnt + 4 - (cnt `mod` 4)) (acc <> Text.replicate (4 - (cnt `mod` 4)) " ")
        _    -> pure acc

-- Try to match the scanners corresponding to any currently open containers.
-- Return remaining text after matching scanners, plus the number of open
-- containers whose scanners did not match.  (These will be closed unless
-- we have a lazy text line.)
tryOpenContainers :: [Container] -> Text -> (Text, Int)
tryOpenContainers cs t =
  case runParser (scanners $ map containerContinue cs) t of
    Right (t', n) -> (t', n)
    Left e        -> error $ "error parsing scanners: " ++ show e
  where
    scanners [] = (,0) <$> untilTheEnd
    scanners (p:ps) = (p *> scanners ps) <|> ((,length (p:ps)) <$> untilTheEnd)

-- Try to match parsers for new containers.  Return list of new
-- container types, and the leaf to add inside the new containers.
tryNewContainers :: Bool -> Bool -> Int -> Text -> (Seq ContainerType, Leaf)
tryNewContainers afterListItem lastLineIsText offset t =
  case runParser newContainers t of
    Right (cs,t') -> (cs, t')
    Left err      -> error (show err)
  where
    newContainers = do
      getPosition >>= \pos -> setPosition pos{ column = offset + 1 }
      regContainers <- Seq.fromList <$> many (containerStart afterListItem lastLineIsText)
      mVerbatimContainer <- optional $ verbatimContainerStart lastLineIsText
      case mVerbatimContainer of
        Just verbatimContainer -- FIXME: Very inefficient append
          -> (regContainers |> verbatimContainer,) <$> textLineOrBlank
        Nothing -> (regContainers,) <$> parseLeaf lastLineIsText

textLineOrBlank :: Parser Leaf
textLineOrBlank = consolidate <$> untilTheEnd
  where consolidate ts | Text.all (==' ') ts = BlankLine ts
                       | otherwise        = TextLine  ts

-- Parse a leaf node.
parseLeaf :: Bool -> Parser Leaf
parseLeaf lastLineIsText = pNonIndentSpaces *> asum
  [ ATXHeading <$> parseAtxHeadingStart <*> parseAtxHeadingContent
  , guard lastLineIsText *> parseSetextToken
  , Rule <$ scanTBreakLine
  , textLineOrBlank
  ]

-- Scanners

scanReference :: Scanner
scanReference = void $ lookAhead (char '[')

-- Scan the beginning of a blockquote:  up to three spaces
-- indent (outside of this scanner), the `>` character, and an optional space.
scanBlockquoteStart :: Scanner
scanBlockquoteStart = char '>' *> tabCrusher *> discardOpt (char ' ')

-- Parse the sequence of `#` characters that begins an ATX
-- header, and return the number of characters.  We require
-- a space after the initial string of `#`s, as not all markdown
-- implementations do. This is because (a) the ATX reference
-- implementation requires a space, and (b) since we're allowing
-- headers without preceding blank lines, requiring the space
-- avoids accidentally capturing a line like `#8 toggle bolt` as
-- a header.
parseAtxHeadingStart :: Parser HeadingLevel
parseAtxHeadingStart = do
  _ <- char '#'
  hashes <- satisfyUpTo 5 (== '#')
  -- hashes must be followed by space unless empty header:
  notFollowedBy (skip ((/= ' ') <&&> (/= '\t')))
  pure $ case (Text.length hashes + 1) of
    1 -> Heading1
    2 -> Heading2
    3 -> Heading3
    4 -> Heading4
    5 -> Heading5
    6 -> Heading6
    _ -> error $ "IMPOSSIBLE HAPPENED: parseAtxHeading parsed more than 6 characters "
parseAtxHeadingContent :: Parser Text
parseAtxHeadingContent = Text.strip . removeATXSuffix <$> untilTheEnd
  where
    removeATXSuffix t =
      case dropTrailingHashes of
        t' | Text.null t' -> t'
             -- an escaped \#
           | Text.last t' == '\\' -> t' <> Text.replicate trailingHashes "#"
           | Text.last t' /= ' ' -> t
           | otherwise -> t'
      where
        dropTrailingSpaces = Text.dropWhileEnd (== ' ') t
        dropTrailingHashes = Text.dropWhileEnd (== '#') dropTrailingSpaces
        trailingHashes     = Text.length dropTrailingSpaces - Text.length dropTrailingHashes

parseSetextToken :: Parser Leaf
parseSetextToken = fmap (uncurry SetextToken) $ withConsumed $ do
  d <- satisfy (\c -> c == '-' || c == '=')
  skipWhile (== d)
  void pBlankline
  pure $ if d == '=' then Heading1 else Heading2

-- Scan a horizontal rule line: "...three or more hyphens, asterisks,
-- or underscores on a line by themselves. If you wish, you may use
-- spaces between the hyphens or asterisks."
scanTBreakLine :: Scanner
scanTBreakLine = do
  c <- satisfy ((== '*') <||> (== '_') <||> (== '-'))
  replicateM_ 2 $ skipWhile ((== ' ') <||> (== '\t')) *> skip (== c)
  skipWhile ((== ' ') <||> (== '\t') <||> (== c))
  endOfInput

-- Parse an initial code fence line, returning
-- the fence part and the rest (after any spaces).
parseCodeFence :: Parser ContainerType
parseCodeFence = do
  col <- column <$> getPosition
  cs <- takeWhile1 (=='`') <|> takeWhile1 (=='~')
  guard $ Text.length cs >= 3
  void pSpaces
  rawattr <- optional (takeWhile1 (\c -> c /= '`' && c /= '~'))
  endOfInput
  pure FencedCode
    { codeStartColumn = col
    , codeFence = cs
    , codeInfo  = rawattr
    }

pHtmlBlockStart :: Bool -> Parser Condition
pHtmlBlockStart lastLineIsText = lookAhead $ do
  discardOpt pNonIndentSpaces
  asum starters
  where
    starters =
      [ condition1 <$ blockStart condition1
      , condition2 <$ blockStart condition2
      , condition3 <$ blockStart condition3
      , condition4 <$ blockStart condition4
      , condition5 <$ blockStart condition5
      , condition6 <$ blockStart condition6
      , condition7 <$ if lastLineIsText then mzero else blockStart condition7
      ]

data Condition =
  Condition
    { blockStart :: Parser ()
    , blockEnd   :: Parser ()
    }

instance Show Condition where
  show _ = "Condition{}"

instance Eq Condition where
  _ == _ = False

lineContains :: Foldable t => t Text -> Parser ()
lineContains terms = do
  line <- Text.toCaseFold <$> takeTill isLineEnding
  guard $ any (`Text.isInfixOf` line) terms

condition1, condition2, condition3, condition4, condition5, condition6, condition7 :: Condition
condition1 = Condition
  { blockStart = do
      _ <- asum $ map stringCaseless ["<script", "<pre", "<style"]
      void pWhitespace <|> void ">" <|> void pLineEnding <|> endOfInput
  , blockEnd = lineContains ["</script>", "</pre>", "</style>"]
  }

condition2 = Condition
  { blockStart = void "<!--"
  , blockEnd = void $ lineContains ["-->"]
  }

condition3 = Condition
  { blockStart = void "<?"
  , blockEnd = void $ lineContains ["?>"]
  }

condition4 = Condition
  { blockStart = void $ "<!" *> satisfy isAsciiUpper
  , blockEnd = void $ lineContains [">"]
  }

condition5 = Condition
  { blockStart = void $ "<![CDATA["
  , blockEnd = void $ lineContains ["]]>"]
  }

condition6 = Condition
  { blockStart = do
      void $ "</" <|> "<"
      tag <- takeTill (isWhitespace <||> (== '/') <||> (== '>'))
      guard $ isBlockHtmlTag (Text.toLower tag)
      void pWhitespace <|> void pLineEnding <|> void ">" <|> void "/>"
  , blockEnd = void pBlankline
  }

condition7 = Condition
  { blockStart = (openTag <|> closeTag) *> (void pWhitespace <|> endOfInput)
  , blockEnd = void pBlankline
  }
  where
    tagName = do
      c <- satisfy (inClass "A-Za-z")
      cs <- takeWhile ((== '-') <||> inClass "A-Za-z0-9")
      guard (Text.cons c cs `notElem` ["script", "style", "pre"])
    attr = pWhitespace *> attrName *> optional attrValueSpec
    attrName = satisfy (inClass "_:A-Za-z") *> skipWhile (inClass "A-Za-z0-9_.:-")
    attrValueSpec = optional pWhitespace *> char '=' *>
                    optional pWhitespace *> attrValue
    attrValue = void unquoted <|> void singleQuoted <|> void doubleQuoted
    unquoted = skipWhile1 (notInClass " \"'=<>`")
    singleQuoted = "'" *> skipWhile (/= '\'') *> "'"
    doubleQuoted = "\"" *> skipWhile (/= '"') *> "\""
    openTag = "<" *> tagName *> many attr *> optional pWhitespace
                                          *> optional "/" *> ">"
    closeTag = "</" *> tagName *> optional pWhitespace *> ">"

-- List of block level tags for HTML 5.
isBlockHtmlTag :: Text -> Bool
isBlockHtmlTag name = Text.toLower name `Set.member` Set.fromList
  [ "address", "article", "aside", "base", "basefont", "blockquote"
  , "body", "caption", "center", "col", "colgroup", "dd", "details"
  , "dialog", "dir", "div", "dl", "dt", "fieldset", "figcaption"
  , "figure", "footer", "form", "frame", "frameset"
  , "h1", "h2", "h3", "h4", "h5", "h6", "head", "header"
  , "hr", "html", "iframe", "legend", "li", "link", "main"
  , "menu", "menuitem", "meta", "nav", "noframes", "ol", "optgroup"
  , "option", "p", "param", "section", "source", "summary", "table"
  , "tbody", "td", "tfoot", "th", "thead", "title", "tr", "track", "ul"
  ]

-- Parse a list marker and return the list type.
--
-- "  1.  Content"
--  ^^  ^^-- contentPadding
--  \\-- markerPadding
parseListMarker :: Bool -> Bool -> Parser ContainerType
parseListMarker afterListItem lastLineIsText = do
  tabCrusher
  markerPadding <- Text.length <$> if afterListItem then pSpaces else pNonIndentSpaces
  ty <- parseBullet <|> parseListNumber lastLineIsText
  -- padding is 1 if list marker followed by a blank line
  -- or indented code.  otherwise it's the length of the
  -- whitespace between the list marker and the following text:
  tabCrusher
  contentPadding <- (1 <$ pBlankline)
                <|> (1 <$ (skip (==' ') *> lookAhead pIndentSpaces))
                <|> (Text.length <$> pSpaces)
  -- text can't immediately follow the list marker:
  guard $ contentPadding > 0
  -- an empty list item cannot interrupt a paragraph
  when lastLineIsText $ notFollowedBy endOfInput
  pure ListItem
    { liType    = ty
    , liPadding = markerPadding + contentPadding + listMarkerWidth ty
    }

listMarkerWidth :: ListType -> Int
listMarkerWidth (Bullet _) = 1
listMarkerWidth (Ordered _ n)
  | n < 10    = 2
  | n < 100   = 3
  | n < 1000  = 4
  | otherwise = 5

-- Parse a bullet and return list type.
parseBullet :: Parser ListType
parseBullet = do
  (bulletType, bulletChar) <- ((Plus,)     <$> char '+')
                          <|> ((Minus,)    <$> char '-')
                          <|> ((Asterisk,) <$> char '*')
  unless (bulletType == Plus) $
    notFollowedBy $ do
      -- hrule
      replicateM_ 2 $ do
        skipWhile ((== ' ') <||> (== '\t'))
        skip (== bulletChar)
      skipWhile (\x -> x == '\t' || x == ' ' || x == bulletChar)
      endOfInput
  return $ Bullet bulletType

-- Parse a list number marker and return list type.
parseListNumber :: Bool -> Parser ListType
parseListNumber lastLineIsText = do
  num :: Integer <- decimal
  when lastLineIsText $
    guard $ num == 1
  guard $ num < (10 ^ (9 :: Integer))
  wrap <- asum [Period <$ char '.', Paren <$ char ')']
  return $ Ordered wrap (fromInteger num)
