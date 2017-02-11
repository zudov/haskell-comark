-- | It mostly contains reexports from the following modules:
--
--     - "Text.Commonmark.Syntax"
--     - "Text.Commonmark.Parser"
--     - "Text.Commonmark.Parser.Options"
--     - "Text.Commonmark.Html"

module Text.Commonmark
    ( -- * Parser
      parse
    , ParserOption(..)
      -- * HTML Rendererer
    , render
      -- * AST types
      -- ** Document
    , Doc(..)
      -- *** Blocks
    , Blocks
    , Block(..)
    , HeadingLevel(..)
    , ListType(..)
    , Delimiter(..)
    , BulletMarker(..)
      -- * Inlines
    , Inlines
    , Inline(..)
    ) where

import Data.Text (Text)

import Text.Commonmark.Html
import Text.Commonmark.Parser
import Text.Commonmark.Parser.Options
import Text.Commonmark.Syntax
