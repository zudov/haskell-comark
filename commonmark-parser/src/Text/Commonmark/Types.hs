module Text.Commonmark.Types
    ( ReferenceMap ) where

import Data.Map  (Map)
import Data.Text (Text)

-- TODO: Probably move it into the Doc
type ReferenceMap = Map Text (Text,Maybe Text)
