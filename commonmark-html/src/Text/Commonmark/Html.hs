{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Text.Commonmark.Html
    ( docToHtml
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Writer.Strict
import           Data.Char
import           Data.Maybe                        (maybeToList)
import           Data.Monoid
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Data.Text.Lazy                    (toStrict)
import           Data.Text.Lazy.Builder            (Builder, fromString,
                                                    fromText, singleton,
                                                    toLazyText)
import           Numeric                           (showIntAtBase)
import           Prelude

import Data.Bits (shiftR, (.&.))

import Text.Commonmark.Syntax

-- | Render a Commonmark document as HTML.
docToHtml :: Doc Text -> Text
docToHtml (Doc bs) = toStrict $ toLazyText $ buildHtml (renderBlocks bs)

type HtmlBuilder = WriterT Builder (State BuilderState) ()
newtype BuilderState = BuilderState { newlineAllowed :: Bool }

type Attribute = (String,Text)

buildAttr :: Attribute -> Builder
buildAttr (name,val) =
    singleton ' ' <> fromString name <> "=\"" <> escapedHtml val <> singleton '"'

type TagName    = String
type TagContent = HtmlBuilder

-- | Build tag with attributes
tagWith :: [Attribute] -> TagName -> TagContent -> HtmlBuilder
tagWith !attrs !t content = do
    let !tagNameBuilder = fromString t
    tell (singleton '<' <> tagNameBuilder <> foldMap buildAttr attrs <> singleton '>')
    allowNL
    content
    tell ("</" <> tagNameBuilder <> singleton '>')
    allowNL

-- | Build tag without attributes
tag :: TagName -> TagContent -> HtmlBuilder
tag = tagWith []

-- | Build void tag
voidTag :: TagName -> HtmlBuilder
voidTag = voidTagWith []

-- | Build void tag with attributes
voidTagWith :: [Attribute] -> TagName -> HtmlBuilder
voidTagWith !attrs !t = do
    tell $ singleton '<' <> fromString t <> foldMap buildAttr attrs <> " />"
    allowNL

allowNL, disallowNL :: HtmlBuilder
allowNL    = lift $ put (BuilderState True)
disallowNL = lift $ put (BuilderState False)

nl :: HtmlBuilder
nl = do
    allowed <- lift $ gets newlineAllowed
    when allowed $ do
        tell "\n"
        disallowNL

escapedText :: Text -> HtmlBuilder
escapedText t = tell (escapedHtml t) >> allowNL

unescapedText :: Text -> HtmlBuilder
unescapedText t = tell (fromText t) >> allowNL

buildHtml :: HtmlBuilder -> Builder
buildHtml m = evalState (execWriterT m) (BuilderState False)

renderBlocks :: Blocks Text -> HtmlBuilder
renderBlocks bs = nl >> mapM_ (\b -> nl >> renderBlock b >> nl) bs

renderBlock :: Block Text -> HtmlBuilder
renderBlock (Para is) = tag "p" (renderInlines is)
renderBlock (Heading n is) = tag hx (renderInlines is)
  where hx = case n of
               Heading1 -> "h1"
               Heading2 -> "h2"
               Heading3 -> "h3"
               Heading4 -> "h4"
               Heading5 -> "h5"
               Heading6 -> "h6"
renderBlock (CodeBlock mInfo t) = tag "pre" $ tagWith args "code" $ escapedText t
    where args = maybeToList (("class",) . lang <$> mInfo)
          lang = ("language-" <>) . T.takeWhile (/= ' ')
renderBlock ThematicBreak = voidTag "hr"
renderBlock (HtmlBlock t) = unescapedText t
renderBlock (Quote bs) = tag "blockquote" $ renderBlocks bs
renderBlock (List listType tight items) = case listType of
    Bullet  _   -> tag "ul" renderedItems
    Ordered _ 1 -> tag "ol" renderedItems
    Ordered _ n -> tagWith [("start", T.pack $ show n)] "ol" renderedItems
  where
    renderedItems = nl >> mapM_ renderItem items

    renderItem bs
      | tight     = tag "li" (mapM_ renderTightBlock bs) >> nl
      | otherwise = tag "li" (when (null bs) disallowNL >> renderBlocks bs) >> nl

    renderTightBlock (Para zs) = mapM_ renderInline zs
    renderTightBlock x         = nl >> renderBlock x >> nl

renderInlines :: Inlines Text -> HtmlBuilder
renderInlines = mapM_ renderInline

renderInline :: Inline Text -> HtmlBuilder
renderInline (Str t)               = escapedText t
renderInline SoftBreak             = tell "\n"
renderInline HardBreak             = voidTag "br" >> nl
renderInline (RawHtml t)           = unescapedText t
renderInline (Emph is)             = tag "em" (renderInlines is)
renderInline (Strong is)           = tag "strong" (renderInlines is)
renderInline (Code t)              = tag "code" (escapedText t)
renderInline (Link is dest title)  = tagWith attrs "a" (renderInlines is)
    where attrs = ("href", encodeHref dest) : maybeToList (("title",) <$> title)
renderInline (Image is dest title) = voidTagWith attrs "img"
    where attrs = ("src", encodeHref dest) : ("alt", foldMap asText is)
                                           : maybeToList (("title",) <$> title)

encodeHref :: Text -> Text
encodeHref = T.concatMap (T.pack . escapeURIChar predicate)
  where predicate c =
           (isAscii c && isAlphaNum c) || (c >= '!' && c <= '_' && (
                c == '-' || c == '_'  || c == '.' || c == '+' || c == '!'
             || c == '*' || c == '\'' || c == '(' || c == ')' || c == ','
             || c == '%' || c == '#'  || c == '@' || c == '?' || c == '='
             || c == ';' || c == ':'  || c == '/' || c == ',' || c == '+'
             || c == '$' || c == '&'))

-- |Escape character if supplied predicate is not satisfied,
--  otherwise return character as singleton string.
--
escapeURIChar :: (Char -> Bool) -> Char -> String
escapeURIChar p c
    | p c       = [c]
    | otherwise = concatMap (\i -> '%' : myShowHex i "") (utf8EncodeChar c)
    where
        myShowHex :: Int -> ShowS
        myShowHex n r =  case showIntAtBase 16 (toChrHex) n r of
            []  -> "00"
            [x] -> ['0',x]
            cs  -> cs
        toChrHex d
            | d < 10    = chr (ord '0' + fromIntegral d)
            | otherwise = chr (ord 'A' + fromIntegral (d - 10))

-- From http://hackage.haskell.org/package/utf8-string
-- by Eric Mertens, BSD3
-- Returns [Int] for use with showIntAtBase
utf8EncodeChar :: Char -> [Int]
utf8EncodeChar = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]

escapedHtml :: Text -> Builder
escapedHtml = fromText . T.replace ">"  "&gt;"
                       . T.replace "<"  "&lt;"
                       . T.replace "\"" "&quot;"
                       . T.replace "&"  "&amp;"
