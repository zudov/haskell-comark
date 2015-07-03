-- | Common AST-constructing functions

module Text.CommonMark.Syntax.Builder
    ( str
    ) where

import           Data.Sequence          (singleton)

import           Text.CommonMark.Syntax

-- | A singleton string
str :: a -> Inlines a
str = singleton . Str
