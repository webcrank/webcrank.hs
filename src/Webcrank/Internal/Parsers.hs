{-# LANGUAGE OverloadedStrings #-}

module Webcrank.Internal.Parsers where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (catMaybes)
import Prelude hiding (takeWhile)

dquote :: Char
dquote = '"'

htab :: Char
htab = '\t'

sp :: Char
sp = ' '

-- stick the hyphen at the front so we can use inClass without a range
vchar :: String
vchar = '-' : [x | x <- ['\32'..'\126'], x /= '-']

-- | Optional whitespace parser
owsP :: Parser ()
owsP = skipWhile (inClass [sp, htab]) <?> "OWS"

tokenP :: Parser ByteString
tokenP = takeWhile1 (inClass tchar) <?> "token"

tchar :: String
tchar = [x | x <- vchar, x /= sp, x `notElem` special]

special :: String
special = "()<>@,;:\\\"/[]?={}"

csl1 :: Parser a -> Parser [a]
csl1 p = (catMaybes .) . (:) <$> x <*> ys >>= failOnEmpty where
  x = optional p
  ys = many (owsP *> char ',' *> optional (owsP *> p))
  failOnEmpty xs = if null xs then fail "csl1" else pure xs

quotedStringP :: Parser ByteString
quotedStringP = dquoteP *> str <* dquoteP <?> "quoted-string" where
  str = B.concat <$> many (qdtextP <|> quotedPairP)
  qdtextP = takeWhile1 (inClass qdtext) <?> "qdtext"
  quotedPairP = char '\\' *> qc <?> "quoted-pair" where
    qc = B.singleton <$> satisfy (inClass (vchar ++ [htab, sp] ++ obsText))

qdtext :: String
qdtext = concat
  [ ['-', htab, sp, '!' ]
  , [x | x <- ['\x23'..'\x7E'], x /= '\\', x/= '-'] -- hyphen should only appear at beginning
  , obsText
  ]

obsText :: String
obsText = ['\x80'..'\xFF']

dquoteP :: Parser Char
dquoteP = char dquote

