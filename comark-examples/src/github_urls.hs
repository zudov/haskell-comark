{-# LANGUAGE OverloadedStrings #-}
-- | Here we define a microsyntax for refering to github repositories,
--   using @LinkReferences@ option.
module Main where

import           Data.Monoid  ((<>))
import           Data.Text    (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

import qualified Comark

main :: IO ()
main =
  Text.interact markdownToHtml

markdownToHtml :: Text -> Text
markdownToHtml =
  Comark.render . Comark.parse [ Comark.LinkReferences githubUrl ]

-- Match url references that look like [github:author/repo]
-- and transform them to proper github links.
githubUrl :: Text -> Maybe (Text, Maybe Text)
githubUrl ref = do
  githubRef <- Text.stripPrefix "github:" ref
  let (username:repo:[]) = Text.splitOn "/" githubRef
      url = "https://github.com/" <> username <> "/" <> repo
      alt = repo <> " on GitHub by @" <> username
  pure (url, Just alt)
