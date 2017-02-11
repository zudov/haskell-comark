{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
-- | Code and data types for parsing emphasis and links
module Comark.Parser.Inline.EmphLink where

import           Data.Text (Text)
import qualified Data.Text as Text

import Comark.Syntax
import Comark.Syntax.Builder

import Data.Sequence (Seq, ViewR(..), singleton, viewr, (<|), (><), (|>))

type DelimStack = Seq Token

data EmphDelim
  = EmphDelim
      { emphIndicator :: EmphIndicator
      , emphLength    :: Int
      , emphCanOpen   :: Bool
      , emphCanClose  :: Bool
      }
  deriving (Show, Eq)

unEmphDelim :: EmphDelim -> Inlines Text
unEmphDelim EmphDelim{..} =
  singleton . Str
    . Text.replicate emphLength
    . Text.singleton . indicatorChar
    $ emphIndicator

data LinkOpen
  = LinkOpen
      { linkOpenerType :: OpenerType
      , linkActive     :: Bool
      , linkLabel      :: Maybe Text
      , linkContent    :: Inlines Text
      }
  deriving (Show, Eq)

unLinkOpen :: LinkOpen -> Inlines Text
unLinkOpen l =
  case linkOpenerType l of
    LinkOpener  -> Str "["
    ImageOpener -> Str "!["
   <| linkContent l

data Token
  = InlineToken (Inlines Text)
  | EmphDelimToken EmphDelim
  | LinkOpenToken LinkOpen
  deriving (Show, Eq)

unToken :: Token -> Inlines Text
unToken (InlineToken is)   = is
unToken (EmphDelimToken e) = unEmphDelim e
unToken (LinkOpenToken l)  = unLinkOpen l

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
deactivate (LinkOpenToken l) =
  LinkOpenToken l
    { linkActive = linkOpenerType l == ImageOpener }
deactivate t = t

addInline :: DelimStack -> Inline Text -> DelimStack
addInline (viewr -> ts :> LinkOpenToken l) i =
  ts |> LinkOpenToken l
          { linkContent = linkContent l |> i }
addInline (viewr -> ts :> InlineToken is) i =
  ts |> InlineToken (is |> i)
addInline ts i =
  ts |> InlineToken (singleton i)

addInlines :: DelimStack -> Inlines Text -> DelimStack
addInlines (viewr -> ts :> InlineToken is) i =
  ts |> InlineToken (is >< i)
addInlines (viewr -> ts :> LinkOpenToken l) i =
  ts |> LinkOpenToken l { linkContent = linkContent l >< i }
addInlines ts i = ts |> InlineToken i
