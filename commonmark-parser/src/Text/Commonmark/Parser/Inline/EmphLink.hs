{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
-- | Code and data types for parsing emphasis and links
module Text.Commonmark.Parser.Inline.EmphLink where

import           Data.Text (Text)
import qualified Data.Text as Text

import Text.Commonmark.Syntax
import Text.Commonmark.Syntax.Builder

import Data.Sequence (Seq, ViewR (..), singleton, viewr, (<|), (><), (|>))

type DelimStack = Seq Token

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

unToken :: Token -> Inlines Text
unToken (InlineToken is) = is
unToken (EmphDelimToken{..}) = str $ Text.replicate dLength $ Text.singleton $ indicatorChar dChar
unToken (LinkOpenToken LinkOpener _ _ c) = Str "[" <| c
unToken (LinkOpenToken ImageOpener _ _ c) = Str "![" <| c

isLinkOpener :: Token -> Bool
isLinkOpener LinkOpenToken{} = True
isLinkOpener _               = False

isEmphDelim :: Token -> Bool
isEmphDelim EmphDelimToken{} = True
isEmphDelim _                = False

data EmphIndicator
  = AsteriskIndicator
  | UnderscoreIndicator
  deriving (Show, Eq)

isAsterisk :: EmphIndicator -> Bool
isAsterisk AsteriskIndicator   = True
isAsterisk UnderscoreIndicator = False

indicatorChar :: EmphIndicator -> Char
indicatorChar AsteriskIndicator   = '*'
indicatorChar UnderscoreIndicator = '_'

data OpenerType
  = LinkOpener
  | ImageOpener
  deriving (Show, Eq)

deactivate :: Token -> Token
deactivate t@LinkOpenToken{..} = t { active = openerType == ImageOpener }
deactivate t = t

addInline :: Seq Token -> Inline Text -> Seq Token
addInline (viewr -> ts :> ot@LinkOpenToken{}) i = ts |> ot { content = content ot |> i }
addInline (viewr -> ts :> InlineToken is) i = ts |> InlineToken (is |> i)
addInline ts i = ts |> InlineToken (singleton i)

addInlines :: Seq Token -> Inlines Text -> Seq Token
addInlines (viewr -> ts :> InlineToken is) i = ts |> InlineToken (is >< i)
addInlines (viewr -> ts :> ot@LinkOpenToken{}) i = ts |> ot { content = content ot >< i }
addInlines ts i = ts |> InlineToken i
