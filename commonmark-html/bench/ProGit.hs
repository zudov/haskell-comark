module Main where

import           Criterion.Main
import qualified Data.Text      as Text
import qualified Data.Text.IO   as Text

import CMark
import Control.DeepSeq
import Text.Commonmark.Html
import Text.Commonmark.Syntax
import Text.Commonmark.TestUtils.CMark

instance NFData Node
instance NFData PosInfo
instance NFData NodeType
instance NFData ListAttributes
instance NFData Text.Commonmark.TestUtils.CMark.ListType
instance NFData DelimType

main :: IO ()
main = do
    file <- Text.readFile "benchinput.md"
    putStrLn "finished reading"
    let node = commonmarkToNode [optNormalize] file
        doc  = nodeToDoc node
    node `deepseq` putStrLn "evaluated node"
    doc  `deepseq` putStrLn "evaluated doc"
    defaultMain [ bench "cmark-hs"   $ nf (nodeToHtml []) node
                , bench "commonmark" $ nf docToHtml doc
                ]
