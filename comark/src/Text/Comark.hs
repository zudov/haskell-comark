-- | It mostly contains reexports from the following modules:
--
--     - "Text.Comark.Syntax"
--     - "Text.Comark.Parser"
--     - "Text.Comark.Parser.Options"
--     - "Text.Comark.Html"

module Text.Comark
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

import Text.Comark.Html
import Text.Comark.Parser
import Text.Comark.Parser.Options
import Text.Comark.Syntax
