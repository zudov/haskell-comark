 module Text.Commonmark
    ( -- * Parser options ("Text.Commonmark.Parser.Options")
      ParseOptions
    , defParseOptions
    , parseOptNormalize
    , parseOptLinkReferences
      -- * Commonmark Parser ("Text.Commonmark.Parser")
    , commonmarkToDoc
      -- * HTML Renderer ("Text.Commonmark.Html")
    , commonmarkToHtml
    ) where

import           Data.Text                      (Text)

import           Text.Commonmark.Html
import           Text.Commonmark.Parser
import           Text.Commonmark.Parser.Options

commonmarkToHtml :: ParseOptions -> Text -> Text
commonmarkToHtml opts = docToHtml . commonmarkToDoc opts

