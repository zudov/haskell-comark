-- | A very simple utility.
--   Takes markdown on stdin spews HTML on stdout.
--   For more extended example have a look at @comark/Main.hs@.
module Main where

import           Data.Text    (Text)
import qualified Data.Text.IO as Text

import qualified Comark

main :: IO ()
main =
  Text.interact markdownToHtml

markdownToHtml :: Text -> Text
markdownToHtml =
  Comark.render . Comark.parse [ Comark.Normalize ]
