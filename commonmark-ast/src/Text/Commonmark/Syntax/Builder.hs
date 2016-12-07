-- | Common AST-constructing functions

module Text.Commonmark.Syntax.Builder
    ( str
    ) where

import Data.Sequence (singleton)

import Text.Commonmark.Syntax

-- | A singleton string
str :: a -> Inlines a
str = singleton . Str
