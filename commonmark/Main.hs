{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative

import qualified Data.Text.IO as T

import System.Environment
import System.Exit
import System.IO

import Text.Commonmark

data Options = Options { outputFormat :: Format
                       , normalize    :: Bool
                       , inputFiles   :: [FilePath]
                       } deriving (Show)

initOpts :: Options
initOpts = Options { outputFormat = Html
                   , normalize    = False
                   , inputFiles   = []
                   }

data Format = Native | Html deriving (Show)

readFormat :: String -> Either String Format
readFormat "html"   = Right Html
readFormat "native" = Right Native
readFormat format   = Left $ "Unknown format " ++ format

parseArgs :: [String] -> Either String Options
parseArgs = parse initOpts
  where
    parse opts = \case
      []                   -> Right opts
      ("-t":[])            -> Left "No argument provided for -t"
      (arg:format:args) | arg == "-t" || arg == "--to"
          -> readFormat format >>= \f -> parse opts { outputFormat = f } args
      ("--normalize":args) -> parse opts { normalize = True } args
      ("--":files)         -> Right opts { inputFiles = files ++ inputFiles opts }
      (('-':_):_)          -> Left usage
      files                -> Right opts { inputFiles = files ++ inputFiles opts }

usage :: String
usage = unlines $
    [ "Usage:    commonmark-hs [OPTIONS*] [FILE*]"
    , "Options:"
    , "  --to, -t FORMAT  Specify output format (html, native)"
    , "  --normalize      Consolidate adjacent text nodes"
    , "  --help, -h       Print usage information"
    ]

main :: IO ()
main = main' =<< either (\err -> hPutStrLn stderr err >> exitFailure) return
             =<< parseArgs <$> getArgs

main' :: Options -> IO ()
main' Options{..} = T.interact (commonmarkToHtml parseOptions)
    where parseOptions = [ Normalize | normalize ]

