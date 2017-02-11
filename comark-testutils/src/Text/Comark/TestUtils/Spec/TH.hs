{-# LANGUAGE TemplateHaskell #-}
module Text.Comark.TestUtils.Spec.TH
    ( specFile
    ) where

import Paths_comark_testutils

import Data.FileEmbed
import Language.Haskell.TH

specFile :: Q Exp
specFile = do
    path <- runIO $ getDataFileName "spec.json"
    embedFile path
