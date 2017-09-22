{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
-- | Link reference-related types and functions
module Comark.Parser.Reference where

import           Data.Char   (isSpace)
import           Data.String
import           Data.Text   (Text)
import qualified Data.Text   as Text

newtype LinkLabel = LinkLabel
  { runLinkLabel :: Text }
  deriving (Show, Eq, Ord, IsString)

-- | Normalizes 'LinkText' to achive a 'LinkLabel' that can be
--   used to validly match against other link labels.
toLinkLabel :: LinkText -> LinkLabel
toLinkLabel =
  LinkLabel
      -- collapse internal whitespace to a single space
    . Text.intercalate " " . filter (not . Text.null) . Text.split isSpace
      -- strip leading and trailing spaces
    . Text.strip
    -- perform "Unicode case fold"
    . Text.toCaseFold
    . runLinkText

newtype LinkText = LinkText
  { runLinkText :: Text }
  deriving (Show, Eq, Ord, IsString)

newtype LinkDestination = LinkDestination
  { runLinkDestination :: Text }
  deriving (Show, Eq, Ord, IsString)

newtype LinkTitle = LinkTitle
  { runLinkTitle :: Text }
  deriving (Show, Eq, Ord, IsString)
