module Text.Commonmark.Parser.Options
    ( ParserOption(..)
    , ParserOptions()
    , parserOptions
    , defParserOptions
    , poNormalize
    , poLinkReferences
    , poParseEmphasis
    ) where

import           Data.Monoid (Endo(Endo, appEndo))
import           Data.Text   (Text)

data ParserOption
  = -- | Consolidate adjacent text nodes
    Normalize
    -- | Allows to predefine
    --   <http://spec.commonmark.org/0.20/#link-reference-definition link reference defenitions>.
    --
    --   References are represented with a mapping from a
    --   <http://spec.commonmark.org/0.20/#link-text link text> to a pair of a
    --   <http://spec.commonmark.org/0.20/#link-destination link destination>
    --   and an optional
    --   <http://spec.commonmark.org/0.20/#link-title link title>.
    --
    --   During parsing the link references defined in a document would be
    --   collected into additional mapping. When link references are being
    --   mapping defined in options takes precedence over mapping found in
    --   the document.
    --
    --   TODO: Examples
  | LinkReferences (Text -> Maybe (Text, Maybe Text))

data ParserOptions = ParserOptions
    { poNormalize      :: Bool
    , poLinkReferences :: Text -> Maybe (Text, Maybe Text)
    , poParseEmphasis  :: Bool
    }

parserOptions :: [ParserOption] -> ParserOptions
parserOptions = ($ defParserOptions) . appEndo . foldMap (Endo . optFn)
  where
    optFn :: ParserOption -> ParserOptions -> ParserOptions
    optFn Normalize o          = o { poNormalize = True }
    optFn (LinkReferences f) o = o { poLinkReferences = f }

defParserOptions :: ParserOptions
defParserOptions = ParserOptions
  { poNormalize      = False
  , poLinkReferences = const Nothing
  , poParseEmphasis  = True
  }
