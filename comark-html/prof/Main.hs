module Main where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

import CMark
import Comark.Html
import Comark.Syntax
import Comark.TestUtils.CMark
import Control.DeepSeq

main :: IO ()
main = do
    file <- Text.readFile "benchinput.md"
    putStrLn "finished reading"
    let node = commonmarkToNode [optNormalize] file
        doc  = nodeToDoc node
        html = docToHtml doc
    doc  `deepseq` putStrLn "evaluated doc"
    html `deepseq` putStrLn "evaluated html"
