{-# LANGUAGE OverloadedStrings #-}

module Webcrank.Parsers where

import Control.Applicative 
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.CaseInsensitive as CI
import Data.Char (ord)
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, maybeToList)
import Prelude hiding (takeWhile)

import Webcrank.Types.Internal
import Webcrank.Types.MediaType

-- | Given the value of an @Accept@ header, produce an ordered list
-- based on the q-values, with the head of the list being the 
-- highest-priority requested type.
parseAccept :: ByteString -> [MediaType]
parseAccept = sort . parseH where
  sort = (fst <$>) . sort'
  sort' = sortBy order where
    order (MediaType prix subx _, qx) (MediaType priy suby _, qy) 
      | qx > qy = GT
      | qx < qy = LT
      | prix == priy && suby == "*" = GT
      | prix == priy && subx == "*" = LT
      | otherwise = EQ
  parseH = either (const []) id . parseOnly (csl mediaTypeP)

parseMediaType :: ByteString -> Maybe (MediaType, Double)
parseMediaType = either (const Nothing) Just . parseOnly mediaTypeP

mediaTypeP :: Parser (MediaType, Double)
mediaTypeP = mk <$> mediaRange <*> params <?> "media-type" where
  mediaRange = wildcard <|> wildSub <|> mtype <?> "media-range"
  wildcard = string "*/*" *> pure ("*", "*")
  wildSub = flip (,) "*" <$> tokenP <* string "/*"
  mtype = (,) <$> tokenP <*> (char '/' *> tokenP)
  params = split <$> many param
  param = (,) . CI.mk <$> (owsP *> char ';' *> owsP *> tokenP) <*> (char '=' *> wordP)
  split ps = (ps', qval) where
    (ps', ext) = break ((== "q") . fst) ps
    qval = listToMaybe ext >>= parseQ . snd
    parseQ = either (const Nothing) Just . parseOnly double
  mk (pri, sub) (ps, q) = (MediaType (CI.mk pri) (CI.mk sub) ps, fromMaybe 1.0 q)

-- TODO should this return `Maybe (NonEmpty (Charset, Double))`?
parseAcceptLang :: ByteString -> [(Charset, Double)]
parseAcceptLang = either (const []) id . parseOnly (csl1 charsetP) where
  charsetP = (,) <$> cp <*> wp
  cp = CI.mk <$> tokenP
  wp = fromMaybe 1.0 <$> optional weightP

parseAcceptEnc :: ByteString -> [(Encoding, Double)]
parseAcceptEnc = either (const []) id . parseOnly (csl codingsP) where
  codingsP = (,) <$> cp <*> wp
  cp = CI.mk <$> (string "identity" <|> tokenP)
  wp = fromMaybe 1.0 <$> optional weightP

alpha :: String
alpha = ['a'..'z'] ++ ['A'..'Z']

cr :: Char
cr = '\r'

crlf :: String
crlf = [cr, lf]

ctl :: String
ctl = '\127' : ['\0' .. '\31']

dquote :: Char
dquote = '"'

hexdig :: String
hexdig = ['0'..'9'] ++ ['A'..'F'] ++ ['a'..'f']

htab :: Char
htab = '\t'

lf :: Char
lf = '\n'

sp :: Char
sp = ' '

-- stick the hyphen at the front so we can use inClass without a range
vchar :: String
vchar = '-' : [x | x <- ['\32'..'\126'], x /= '-']

-- | Optional whitespace parser
owsP :: Parser ()
owsP = skipWhile (inClass [sp, htab]) <?> "OWS"

-- | Required whitespace parser
rwsP :: Parser ByteString
rwsP = takeWhile1 (inClass [sp, htab]) <?> "RWS"

wordP :: Parser ByteString
wordP = tokenP <|> quotedStringP <?> "word"

tokenP :: Parser ByteString
tokenP = takeWhile1 (inClass tchar) <?> "token"

tchar :: String
tchar = [x | x <- vchar, x /= sp, x `notElem` special]

special :: String
special = "()<>@,;:\\\"/[]?={}"

csl :: Parser a -> Parser [a]
csl = (concat . maybeToList <$>) . optional . csl1

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

weightP :: Parser Double
weightP = owsP *> char ';' *> owsP *> string "q=" *> qvalP

qvalP :: Parser Double
qvalP = v <|> one where
  v = zero *> (fromMaybe 0 <$> optional (dot *> dec))
  dec = (\x y z -> fromIntegral (x + y + z) / 1000.0) <$> d 100 <*> d 10 <*> d 1
  one = char '1' *> optional (dot *> optional zero *> optional zero *> optional zero) *> pure 1
  zero = char '0'
  dot = char '.'
  toInt = subtract 48 . ord
  d x = maybe 0 ((*x) . toInt) <$> optional digit
