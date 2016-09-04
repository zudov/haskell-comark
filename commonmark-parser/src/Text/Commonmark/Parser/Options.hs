module Text.Commonmark.Parser.Options
    ( ParseOptions
    , defParseOptions
    , parseOptNormalize
    , parseOptLinkReferences
    ) where

import           Data.Text (Text)

data ParseOptions = ParseOptions
    { -- | Consolidate adjacent text nodes
      parseOptNormalize      :: Bool
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
    , parseOptLinkReferences :: Text -> Maybe (Text, Maybe Text)
    }

defParseOptions :: ParseOptions
defParseOptions = ParseOptions
  { parseOptNormalize = False
  , parseOptLinkReferences = const Nothing
  }
