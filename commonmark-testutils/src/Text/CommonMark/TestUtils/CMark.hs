{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Text.CommonMark.TestUtils.CMark
    ( module CMark
    , nodeToDoc
    , nodeToBlock
    , nodeToInline
    ) where

import           CMark
import           Data.Monoid
import qualified Data.Sequence          as S
import           Data.Text              (Text, stripEnd)

import           Text.CommonMark.Syntax

-- | Converts a cmark's AST to commonmark's AST
nodeToDoc :: Node -> Doc Text
nodeToDoc (Node _ DOCUMENT ns) = Doc $ S.fromList $ map nodeToBlock ns
nodeToDoc _ = error "Top-level node must be DOCUMENT"

nodeToBlock :: Node -> Block Text
nodeToBlock (Node _ THEMATIC_BREAK []) = ThematicBreak
nodeToBlock (Node _ THEMATIC_BREAK _)  = error "THEMATIC_BREAK node has children"
nodeToBlock (Node _ PARAGRAPH ns)   = Para $ S.fromList $ map nodeToInline ns
nodeToBlock (Node _ BLOCK_QUOTE ns) = Quote $ S.fromList $ map nodeToBlock ns
nodeToBlock (Node _ (HTML_BLOCK html) []) = HtmlBlock $ stripEnd html
nodeToBlock (Node _ (HTML_BLOCK _) _)     = error "HTML node has children"
nodeToBlock (Node _ (CODE_BLOCK i c) []) = CodeBlock (monoidToMaybe i) c
nodeToBlock (Node _ CODE_BLOCK{} _)    = error "CODE_BLOCK has children"
nodeToBlock (Node _ (HEADING l) ns) = Heading h $ S.fromList $ map nodeToInline ns
  where h = case l of
              1 -> Heading1
              2 -> Heading2
              3 -> Heading3
              4 -> Heading4
              5 -> Heading5
              6 -> Heading6
              _ -> error "HEADING has invalid level"
nodeToBlock (Node _ (LIST ListAttributes{..}) ns) =
    List listType' listTight $ map itemToBlocks ns
    where listType' = case (listType,listDelim) of
                (BULLET_LIST,_)             -> Bullet Asterisk
                (ORDERED_LIST,PERIOD_DELIM) -> Ordered Period listStart
                (ORDERED_LIST,PAREN_DELIM)  -> Ordered Paren listStart
nodeToBlock (Node _ type_ _) = error $ show type_ ++ " isn't a block node"

itemToBlocks :: Node -> Blocks Text
itemToBlocks (Node _ ITEM ns) = S.fromList $ map nodeToBlock ns
itemToBlocks (Node _ type_ _ ) = error $ show type_ ++ " isn't an ITEM"

nodeToInline :: Node -> Inline Text
nodeToInline (Node _ (TEXT t) []) = Str t
nodeToInline (Node _ (TEXT _) _)  = error "TEXT has children"
nodeToInline (Node _ SOFTBREAK []) = SoftBreak
nodeToInline (Node _ SOFTBREAK _) = error "SOFTBREAK has children"
nodeToInline (Node _ LINEBREAK []) = HardBreak
nodeToInline (Node _ LINEBREAK _) = error "LINEBREAK has children"
nodeToInline (Node _ (HTML_INLINE html) []) = RawHtml html
nodeToInline (Node _ (HTML_INLINE _) _) = error "INLINE_HTML has children"
nodeToInline (Node _ (CODE c) []) = Code c
nodeToInline (Node _ (CODE _) _)  = error "CODE has children"
nodeToInline (Node _ EMPH ns) = Emph $ S.fromList $ map nodeToInline ns
nodeToInline (Node _ STRONG ns) = Strong $ S.fromList $ map nodeToInline ns
nodeToInline (Node _ (LINK d t) ns) = Link (S.fromList $ map nodeToInline ns) d (monoidToMaybe t)
nodeToInline (Node _ (IMAGE d t) ns) = Image (S.fromList $ map nodeToInline ns) d (monoidToMaybe t)
nodeToInline (Node _ type_ _) = error $ show type_ ++ " is not a block node"

monoidToMaybe :: (Eq a, Monoid a) => a -> Maybe a
monoidToMaybe a
    | mempty == a = Nothing
    | otherwise   = Just a
