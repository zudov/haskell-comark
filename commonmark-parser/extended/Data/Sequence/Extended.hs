module Data.Sequence.Extended
  ( module Data.Sequence
  , module Data.Sequence.Extended
  ) where

import Data.Sequence

-- | Finds the leftmost element that satisfies the predicate.
--   Returns (prefix, element, suffix).
findl :: (a -> Bool) -> Seq a -> Maybe (Seq a, a, Seq a)
findl p s =
  case breakl p s of
    (prefix, rest) ->
      case viewl rest of
        EmptyL      -> Nothing
        a :< suffix -> Just (prefix, a, suffix)

-- | Finds the rightmost element that satisfies the predicate.
--   Returns (suffix, element, prefix).
findr :: (a -> Bool) -> Seq a -> Maybe (Seq a, a, Seq a)
findr p s =
  case breakr p s of
    (suffix, rest) ->
      case viewr rest of
        EmptyR      -> Nothing
        prefix :> a -> Just (suffix, a, prefix)
