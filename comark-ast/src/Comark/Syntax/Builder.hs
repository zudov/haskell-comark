-- | Common AST-constructing functions

module Comark.Syntax.Builder
    ( str
    ) where

import Data.Sequence (singleton)

import Comark.Syntax

-- | A singleton string
str :: a -> Inlines a
str = singleton . Str
