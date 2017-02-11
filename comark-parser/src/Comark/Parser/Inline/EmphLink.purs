-- | Code and data types for parsing emphasis and links
module Text.Commonmark.Parser.Inline.EmphLink where

data Token
  = InlineToken (Inlines Text)
  | EmphDelimToken
      { dChar     :: EmphIndicator
      , dLength   :: Int
      , dCanOpen  :: Bool
      , dCanClose :: Bool
      }
  | LinkOpenToken
      { openerType :: OpenerType
      , active     :: Bool
      , refLabel   :: Maybe Text
      , content    :: Inlines Text
      }
  deriving (Show, Eq)

data EmphIndicator
  = AsteriskIndicator
  | UnderscoreIndicator
  deriving (Show, Eq)


