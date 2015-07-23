{-# LANGUAGE OverloadedStrings #-}
module Text.CommonMark.Parser.Util where

import           Control.Applicative
import           Control.Bool
import           Data.Char
import qualified Data.Map                          as M
import           Data.Monoid
import qualified Data.Set                          as S
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Prelude                           hiding (takeWhile)

import           Text.CommonMark.ParserCombinators
import           Text.CommonMark.Types

type Scanner = Parser ()

-- | A newline (U+000A), carriage return (U+000D), or carriage return + newline.
lineEnding :: Scanner
lineEnding = (discard $ char '\n') <|> (discard $ "\r\n")
                                   <|> (discard $ char '\r')

nfb :: Parser a -> Parser ()
nfb = notFollowedBy

nfbChar :: Char -> Scanner
nfbChar c = nfb (skip (== c))

isAsciiPunctuation :: Char -> Bool
isAsciiPunctuation = inClass "!\"#$%&'()*+,./:;<=>?@[\\]^_`{|}~-"

-- Scan a specified character.
scanChar :: Char -> Scanner
scanChar c = skip (== c)

-- Scan a specified character.
scanChars :: Char -> Scanner
scanChars c = skipMany (scanChar c)

-- Scan four spaces.
scanIndentSpace :: Scanner
scanIndentSpace = () <$ count 4 (skip (==' '))

scanSpacesToColumn :: Int -> Scanner
scanSpacesToColumn col = do
  currentCol <- column <$> getPosition
  case col - currentCol of
       n | n >= 1 -> () <$ (count n (skip (==' ')))
         | otherwise -> return ()

scanSpacesUpToColumn :: Int -> Scanner
scanSpacesUpToColumn col = do
  currentCol <- column <$> getPosition
  case col - currentCol of
       n | n >= 1 -> () <$ (count n (discardOpt $ skip (==' ')))
         | otherwise -> return ()

-- Scan 0-3 spaces.
scanNonindentSpace :: Scanner
scanNonindentSpace = () <$ upToCountChars 3 (==' ')

countNonindentSpace :: Parser Int
countNonindentSpace = T.length <$> upToCountChars 3 (== ' ')

upToCountChars :: Int -> (Char -> Bool) -> Parser Text
upToCountChars cnt f =
  scan 0 (\n c -> if n < cnt && f c then Just (n+1) else Nothing)

-- Scan a blankline.
scanBlankline :: Scanner
scanBlankline = scanSpaces *> endOfInput

-- Scan 0 or more spaces
scanSpaces :: Scanner
scanSpaces = skipWhile (==' ')

countSpaces :: Parser Int
countSpaces = T.length <$> takeWhile (== ' ')

-- Scan 0 or more spaces, and optionally a newline
-- and more spaces.
scanSpnl :: Scanner
scanSpnl = scanSpaces *> option () (char '\n' *> scanSpaces)

-- | A [whitespace character] as in spec
isWhitespace :: Char -> Bool
isWhitespace = (`elem` (" \t\n\r\f\v" :: [Char]))

-- | [whitespace] as in spec
pWhitespace :: Parser Text
pWhitespace = takeWhile1 isWhitespace

skipWhitespace :: Scanner
skipWhitespace = skipWhile1 isWhitespace

-- | optional scanWhitespace (including up to one line ending)
scanWhitespaceNL :: Scanner
scanWhitespaceNL =
    option () scanWhitespaceNoNL *> option () (lineEnding *> option () scanWhitespaceNoNL)

scanWhitespaceNoNL :: Scanner
scanWhitespaceNoNL = skipWhile1 (isWhitespace <&&> (`notElem` ("\r\n" :: [Char])))

skipWhitespaceNoNL :: Scanner
skipWhitespaceNoNL = skipWhile (isWhitespace <&&> (`notElem` ("\r\n" :: [Char])))

-- | [unicode whitespace] as in spec
isUnicodeWhitespace :: Char -> Bool
isUnicodeWhitespace = isSpace

pUnicodeWhitespace :: Parser Text
pUnicodeWhitespace = takeWhile1 isWhitespace

normalizeReference :: Text -> Text
normalizeReference = T.toCaseFold . T.concat . T.split isSpace

lookupLinkReference :: ReferenceMap
                    -> Text                -- reference label
                    -> Maybe (Text, Maybe Text)  -- (url, title)
lookupLinkReference refmap key = M.lookup (normalizeReference key) refmap

parenthesize :: Text -> Text
parenthesize x = "(" <> x <> ")"

bracketize :: Text -> Text
bracketize x = "[" <> x <> "]"

monoidToMaybe :: (Eq a, Monoid a) => a -> Maybe a
monoidToMaybe a
    | a == mempty = Nothing
    | otherwise = Just a

-- | TODO: Use Data.Set
isValidScheme :: Text -> Bool
isValidScheme s = S.member (T.toLower s) $ S.fromList
    ["coap","doi","javascript","aaa","aaas","about","acap","cap","cid"
    ,"crid","data","dav","dict","dns","file","ftp","geo","go","gopher"
    ,"h323","http","https","iax","icap","im","imap","info","ipp","iris"
    ,"iris.beep","iris.xpc","iris.xpcs","iris.lwz","ldap","mailto","mid"
    ,"msrp","msrps","mtqp","mupdate","news","nfs","ni","nih","nntp"
    ,"opaquelocktoken","pop","pres","rtsp","service","session","shttp"
    ,"sieve","sip","sips","sms","snmp,soap.beep","soap.beeps","tag","tel"
    ,"telnet","tftp","thismessage","tn3270","tip","tv","urn","vemmi","ws"
    ,"wss","xcon","xcon-userid","xmlrpc.beep","xmlrpc.beeps","xmpp","z39.50r"
    ,"z39.50s","adiumxtra","afp","afs","aim","apt,attachment","aw","beshare"
    ,"bitcoin","bolo","callto","chrome,chrome-extension","com-eventbrite-attendee"
    ,"content","cvs,dlna-playsingle","dlna-playcontainer","dtn","dvb","ed2k"
    ,"facetime","feed","finger","fish","gg","git","gizmoproject","gtalk"
    ,"hcp","icon","ipn","irc","irc6","ircs","itms","jar","jms","keyparc"
    ,"lastfm","ldaps","magnet","maps","market,message","mms","ms-help"
    ,"msnim","mumble","mvn","notes","oid","palm","paparazzi","platform"
    ,"proxy","psyc","query","res","resource","rmi","rsync","rtmp","secondlife"
    ,"sftp","sgn","skype","smb","soldat","spotify","ssh","steam","svn"
    ,"teamspeak","things","udp","unreal","ut2004","ventrilo","view-source"
    ,"webcal","wtai","wyciwyg","xfire","xri","ymsgr"]
