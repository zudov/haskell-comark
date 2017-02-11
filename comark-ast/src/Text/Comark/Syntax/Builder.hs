-- | Common AST-constructing functions

module Text.Comark.Syntax.Builder
    ( str
    ) where

import Data.Sequence (singleton)

import Text.Comark.Syntax

-- | A singleton string
str :: a -> Inlines a
str = singleton . Str
