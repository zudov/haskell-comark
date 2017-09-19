-- | This module demonstraints how AST can be traversed generically.
--   We turn code blocks into html with appropriate classes/styles set.
--
--   Note: Alternatively you code highlight client-side with javascript.
--   Codeblocks have @class="language-{lang}"@ which would be picked up
--   by most highlighting libraries.
module Main where

import qualified Data.Generics                 as Generics
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified Data.Text.Lazy                as Text.Lazy
import qualified Text.Blaze.Html.Renderer.Text as Blaze

import qualified Text.Highlighting.Kate as Kate

import qualified Comark

main :: IO ()
main =
  Text.interact markdownToHtml

markdownToHtml :: Text -> Text
markdownToHtml =
  Comark.render . highlightDoc . Comark.parse []

-- | Turn code blocks into html with appropriate classes/styles set.
highlightDoc :: Comark.Doc Text -> Comark.Doc Text
highlightDoc =
  -- Generic transformation from "syb" to map over all
  -- the blocks. No matter how nested they are.
  Generics.everywhere (Generics.mkT highlightBlock)

-- | Convert a single block into highlighted html.
highlightBlock :: Comark.Block Text -> Comark.Block Text
highlightBlock (Comark.CodeBlock (Just lang) code) =
  Comark.HtmlBlock
    $ Text.Lazy.toStrict
    $ Blaze.renderHtml
    $ Kate.formatHtmlBlock Kate.defaultFormatOpts
    $ Kate.highlightAs (Text.unpack lang) (Text.unpack code)
highlightBlock block = block

