{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Comark.Html.Sanitize where

import qualified Data.Generics as Generics
import           Data.Sequence as Seq
import           Data.Text     (Text)
import qualified Data.Text     as Text

import Comark.Syntax

removeHtml :: Doc Text -> Doc Text
removeHtml =
  removeUnsafeLinks . removeHtmlBlocks

removeRawHtml :: Doc Text -> Doc Text
removeRawHtml =
  Generics.everywhere (Generics.mkT filterInlines)
  where
    filterInlines :: Inlines Text -> Inlines Text
    filterInlines = Seq.filter isRawHtml

    isRawHtml =
      \case RawHtml _ -> True
            _         -> False

removeHtmlBlocks :: Doc Text -> Doc Text
removeHtmlBlocks =
  Generics.everywhere (Generics.mkT filterBlocks)
  where
    filterBlocks :: Blocks Text -> Blocks Text
    filterBlocks = Seq.filter isHtmlBlock

    isHtmlBlock =
      \case HtmlBlock _ -> True
            _           -> False

removeUnsafeLinks :: Doc Text -> Doc Text
removeUnsafeLinks =
  Generics.everywhere (Generics.mkT withInline)
  where
    withInline =
      \case
        Link is dest title
          | isUnsafeDestination dest -> Link is "" title
        Image is dest title
          | isUnsafeDestination dest -> Image is "" title
        b -> b

isUnsafeDestination :: Text -> Bool
isUnsafeDestination t =
   safeDataProtocol || not unsafeProtocol
  where
    unsafeProtocol =
      and $ map (`Text.isPrefixOf` Text.toLower t)
        [ "javascript:"
        , "vbscript:"
        , "file:"
        , "data:"
        ]
    safeDataProtocol =
      and $ map (`Text.isPrefixOf` Text.toLower t)
        [ "data:image/png"
        , "data:image/jpeg"
        , "data:image/webp"
        , "data:image/gif"
        ]



