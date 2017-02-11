-- | It mostly contains reexports from the following modules:
--
--     - "Comark.Syntax"
--     - "Comark.Parser"
--     - "Comark.Parser.Options"
--     - "Comark.Html"

module Comark
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

import Comark.Html
import Comark.Parser
import Comark.Parser.Options
import Comark.Syntax
