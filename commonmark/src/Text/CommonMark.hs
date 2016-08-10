module Text.CommonMark
    ( commonmarkToHtml
    , module Text.CommonMark.Parser.Options
    ) where

import           Data.Text                      (Text)

import           Text.CommonMark.Html
import           Text.CommonMark.Parser
import           Text.CommonMark.Parser.Options

commonmarkToHtml :: ParseOptions -> Text -> Text
commonmarkToHtml opts = docToHtml . commonmarkToDoc opts
