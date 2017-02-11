module Main where

import           Criterion.Main
import qualified Data.Text      as Text
import qualified Data.Text.IO   as Text

import qualified CMark
import           Comark.Html            as Comark
import           Comark.Syntax          as Comark
import           Comark.TestUtils.CMark (ListType, nodeToDoc)
import           Control.DeepSeq

instance NFData CMark.Node
instance NFData CMark.PosInfo
instance NFData CMark.NodeType
instance NFData CMark.ListAttributes
instance NFData Comark.TestUtils.CMark.ListType
instance NFData CMark.DelimType

main :: IO ()
main = do
    input <- Text.readFile "benchinput.md"
    putStrLn "finished reading"
    let node = CMark.commonmarkToNode [CMark.optNormalize] input
        doc  = nodeToDoc node
    node `deepseq` putStrLn "evaluated node"
    doc  `deepseq` putStrLn "evaluated doc"
    defaultMain
      [ bench "cmark-hs" $ nf (CMark.nodeToHtml []) node
      , bench "comark"   $ nf Comark.render doc
      ]
