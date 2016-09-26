-- | This module mostly contains reexports from the following modules:
--
--     - "Text.Commonmark.Parser.Options"
--     - "Text.Commonmark.Parser"
--     - "Text.Commonmark.Html"

module Text.Commonmark
    ( -- * Parser options
      ParseOptions
    , defParseOptions
    , parseOptNormalize
    , parseOptLinkReferences
      -- * Commonmark Parser
    , commonmarkToDoc
      -- * HTML Rendererer
    , docToHtml
      -- * Common compositions
    , commonmarkToHtml
    ) where

import           Data.Text                      (Text)

import           Text.Commonmark.Html
import           Text.Commonmark.Parser
import           Text.Commonmark.Parser.Options

commonmarkToHtml :: ParseOptions -> Text -> Text
commonmarkToHtml opts = docToHtml . commonmarkToDoc opts

