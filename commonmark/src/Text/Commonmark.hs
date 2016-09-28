-- | This module mostly contains reexports from the following modules:
--
--     - "Text.Commonmark.Parser.Options"
--     - "Text.Commonmark.Parser"
--     - "Text.Commonmark.Html"

module Text.Commonmark
    ( -- * Parser
     commonmarkToDoc
      -- ** Parser options
    , ParserOption(..)
      -- * HTML Rendererer
    , docToHtml
      -- * Common compositions
    , commonmarkToHtml
    ) where

import           Data.Text                      (Text)

import           Text.Commonmark.Html
import           Text.Commonmark.Parser
import           Text.Commonmark.Parser.Options

-- | Parse Commonmark document and render it as HTML.
commonmarkToHtml :: [ParserOption] -> Text -> Text
commonmarkToHtml opts = docToHtml . commonmarkToDoc opts