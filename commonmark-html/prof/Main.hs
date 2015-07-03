module Main where

import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T

import           CMark
import           Control.DeepSeq
import           Text.CommonMark.Html
import           Text.CommonMark.Syntax
import           Text.CommonMark.TestUtils.CMark

main :: IO ()
main = do
    file <- T.readFile "benchinput.md"
    putStrLn "finished reading"
    let node = commonmarkToNode [optNormalize] file
        doc  = nodeToDoc node
        html = docToHtml doc
    doc  `deepseq` putStrLn "evaluated doc"
    html `deepseq` putStrLn "evaluated html"
