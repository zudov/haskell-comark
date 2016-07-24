{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}

-- | A definition of CommonMark's AST

module Text.CommonMark.Syntax
    ( Doc(..)
    -- * Block Elements
    , Blocks
    , Block(..)
    , ListType(..)
    , Delimiter(..)
    , BulletMarker(..)
    -- * Inline Elements
    , Inlines
    , Inline(..)
    , normalize
    , asText
    ) where

import           Control.DeepSeq (NFData)
import           Data.Data       (Data, Typeable)
import           Data.Monoid
import           Data.Sequence   (Seq, ViewL (..), viewl, (<|))
import           Data.String     (IsString)
import           GHC.Generics    (Generic)


-- | A Document
newtype Doc t = Doc (Blocks t) deriving (Show, Read, Eq, Typeable, Data, Generic, Functor, Foldable, Traversable)

instance NFData t => NFData (Doc t)

instance Monoid t => Monoid (Doc t) where
    mempty = Doc mempty
    (Doc bs1 ) `mappend` (Doc bs2) = Doc (bs1 `mappend` bs2)

type Blocks t = Seq (Block t)

-- | Block elements
data Block t =
    ThematicBreak -- ^ Thematic break
  | Header Int (Inlines t) -- ^ Header: level, sequnce of inlines that define content
  | CodeBlock (Maybe t) t -- ^ Block of code: info string, literal content
  | HtmlBlock t -- ^ Raw HTML Block
  | Para (Inlines t)  -- ^ Paragraph (a grouped sequence of inlines)
  | Quote (Blocks t) -- ^ Block Quote (a quoted sequence of blocks)
   -- ^ List: Type of the list, tightness, a sequnce of blocks (list item)
  | List ListType Bool [Blocks t]
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic, Functor, Foldable, Traversable)

instance (NFData t) => NFData (Block t)

data ListType = Ordered Delimiter Int
              | Bullet BulletMarker
              deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

instance NFData ListType

data Delimiter = Period
               | Paren
               deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)
instance NFData Delimiter

data BulletMarker = Minus    -- ^ '-'
                  | Plus     -- ^ '+'
                  | Asterisk -- ^ '*'
                  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

instance NFData BulletMarker

type Inlines t = Seq (Inline t)

-- | Inline elements
data Inline t =
    Str t            -- ^ Text (string)
  | Code t           -- ^ Inline code
  | Emph (Inlines t) -- ^ Emphasized text (a sequence of inlines)
  | Strong (Inlines t) -- ^ Strongly emphasized text (a sequence of inlines)
    -- ^ Hyperlink: visible link text (sequence of inlines), destination, title
  | Link (Inlines t) t (Maybe t) -- TODO: special types
    -- ^ Image hyperlink: image description, destination, title
  | Image (Inlines t) t (Maybe t) -- TODO: special types
  | RawHtml t -- ^ Inline Raw HTML tag
  | SoftBreak
  | HardBreak
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic, Functor, Foldable, Traversable)

instance NFData t => NFData (Inline t)


-- | Consolidate adjacent text nodes
normalize :: Monoid t => Inlines t -> Inlines t
normalize inlines = case viewl inlines of
    Str t       :< (viewl -> Str ts :< is) -> normalize (Str (t <> ts) <| is)
    Image i u t :< is -> Image  (normalize i) u t  <| normalize is
    Link i u t  :< is -> Link   (normalize i) u t  <| normalize is
    Emph i      :< is -> Emph   (normalize i)      <| normalize is
    Strong i    :< is -> Strong (normalize i)      <| normalize is
    i           :< is -> i                         <| normalize is
    EmptyL            -> mempty


-- | Extract textual content from an inline.
--   Note that it extracts only the 'primary' content (the one that is shown in
--   first place). For example it wouldn't extract an URL from the link.
asText :: (Monoid a, IsString a) => Inline a -> a
asText (Str t) = t
asText (Emph is) = foldMap asText is
asText (Strong is) = foldMap asText is
asText (Code t) = t
asText (Link is _ _) = foldMap asText is
asText (Image is _ _) = foldMap asText is
asText (RawHtml t) = t
asText SoftBreak = " "
asText HardBreak = "\n"
