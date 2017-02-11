{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative

import           Data.Monoid  ((<>))
import           Data.Text    (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

import System.Environment
import System.Exit
import System.IO

import qualified Text.Commonmark as Commonmark

main :: IO ()
main = do
  eOpts <- parseArgs <$> getArgs
  case eOpts of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right opts ->
      Text.interact (processor opts)

processor :: Options -> Text -> Text
processor Options{..} = render . Commonmark.parse [ Commonmark.Normalize | normalize ]
  where
    render =
      case outputFormat of
        Native -> Text.pack . show
        Html   -> Commonmark.render

data Options = Options
  { outputFormat :: Format
  , normalize    :: Bool
  , inputFiles   :: [FilePath]
  }
  deriving (Show)

initOpts :: Options
initOpts = Options
  { outputFormat = Html
  , normalize    = False
  , inputFiles   = mempty
  }

usage :: String
usage = unlines $
    [ "Usage:    commonmark-hs [OPTIONS*] [FILE*]"
    , "Options:"
    , "  --to, -t FORMAT  Specify output format (html, native)"
    , "  --normalize      Consolidate adjacent text nodes"
    , "  --help, -h       Print usage information"
    ]

parseArgs :: [String] -> Either String Options
parseArgs = parse initOpts
  where
    parse opts = \case
      []                   -> Right opts
      ("-t":[])            -> Left "No argument provided for -t"
      (arg:format:args) | arg == "-t" || arg == "--to"
          -> readFormat format >>= \f -> parse opts { outputFormat = f } args
      ("--normalize":args) -> parse opts { normalize = True } args
      ("--":files)         -> Right opts { inputFiles = files <> inputFiles opts }
      (('-':_):_)          -> Left usage
      files                -> Right opts { inputFiles = files <> inputFiles opts }


data Format = Native | Html deriving (Show)

readFormat :: String -> Either String Format
readFormat "html"   = Right Html
readFormat "native" = Right Native
readFormat format   = Left $ "Unknown format " <> format

