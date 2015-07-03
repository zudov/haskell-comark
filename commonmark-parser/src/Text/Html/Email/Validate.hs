{-# LANGUAGE OverloadedStrings #-}
module Text.Html.Email.Validate
    ( isValidEmail
    ) where

import           Control.Monad
import           Data.Either                       (isRight)
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Text.CommonMark.ParserCombinators

isValidEmail :: Text -> Bool
isValidEmail = isRight . parse (scanEmail *> endOfInput)

scanEmail :: Parser ()
scanEmail = local *> char '@' *> domain

local :: Parser ()
local = skipWhile1 (inClass "A-Za-z0-9!#$%&'*+/=?^_`{|}~.-")

domain :: Parser ()
domain = () <$ label `sepBy1` char '.'

label :: Parser ()
label = do
    lbl <- T.intercalate "-" <$> takeWhile1 (inClass "A-Za-z0-9") `sepBy1` char '-'
    when (T.length lbl > 63) $ fail "Label is too long"
