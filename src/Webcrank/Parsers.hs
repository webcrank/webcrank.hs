{-# LANGUAGE OverloadedStrings #-}

module Webcrank.Parsers
--   ( parseAcceptHeader
--   , parseMediaType
--   , parseConnegHeader 
--   , parseEtags
--   ) where
  where

import Control.Applicative (Applicative, (<|>), (<$>), (<*>), (<*), (*>), many, (<$))
import Control.Arrow (first)
import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly, sepBy, sepBy1, takeWhile1, char, (.*>), option, double, skipSpace, anyChar)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.List (find, sortBy)
import Data.Monoid ((<>))

import Webcrank.Types.MediaType

-- | Given the value of an @Accept@ header, produce an ordered list
-- based on the q-values, with the head of the list being the 
-- highest-priority requested type.
parseAcceptHeader :: ByteString -> [MediaType]
parseAcceptHeader = order . parse where
  order = (fst <$>) . sortq . (prioritizeMediaType <$>) 
  sortq = sortBy ord where
    ord (MediaType prix subx _, qx) (MediaType priy suby _, qy) 
      | qx > qy = GT
      | qx < qy = LT
      | prix == priy && suby == "*" = GT
      | prix == priy && subx == "*" = LT
      | otherwise = EQ
  parse = either (const []) id . parseOnly acceptHeaderP
  acceptHeaderP = mediaTypeP `sepBy1` comma

prioritizeMediaType :: MediaType -> (MediaType, Double)
prioritizeMediaType (MediaType pri sub ps) = (mt, q) where
  mt = MediaType pri sub (filter (not . isQ) ps)
  q = maybe 1.0 (parseQ . snd) (find isQ ps)
  isQ = (== "q") . fst
  parseQ "1" = 1
  parseQ v = either (const 1) id (parseOnly double v)

parseMediaType :: ByteString -> Maybe MediaType
parseMediaType = either (const Nothing) Just . parseOnly mediaTypeP

mediaTypeP :: Parser MediaType
mediaTypeP = do
  let mediaRange = skipSpace *> (wildcard <|> wildSub <|> mtype) <* skipSpace
      wildcard  = char '*' >> slash >> char '*' >> return ("*", "*")
      wildSub   = (\p -> (p, "*")) <$> token <* (slash >> char '*')
      mtype     = (,) <$> token <*> (slash *> token)
      param     = (,) <$> token <*> (equal *> paramVal)
      paramVal  = quotedString <|> token
  (pri, sub) <- mediaRange
  ps         <- many (semicolon >> param)
  return $ MediaType (CI.mk pri) (CI.mk sub) (first CI.mk <$> ps)

parseConnegHeader :: ByteString -> [(CI ByteString, Double)]
parseConnegHeader = either (const []) id . parseOnly connegHeader where
  connegHeader = connegChoice `sepBy1` comma
  connegChoice = do
    accept <- token
    skipSpace
    q      <- option 1.0 (";" .*> ("q" .*> ("=" .*> double)))
    return (CI.mk accept, q)

parseEtags :: ByteString -> [ByteString]
parseEtags = either (const []) id . parseOnly etags where
  etags = etag `sepBy` comma
  etag = "W/" .*> quotedString <|> quotedString

token :: Parser ByteString
token = takeWhile1 (`notElem` cs) where
  cs = ['\0'..'\31'] ++ "()<>@,;:\\\"/[]?={} \t" ++ ['\128'..'\255']

quotedString :: Parser ByteString
quotedString = char '"' *> go where
  go = escaped <|> end <|> taking
  escaped = B.cons <$> (char '\\' >> anyChar) <*> go
  end = B.empty <$ char '"' 
  taking = (<>) <$> takeWhile1 (`notElem` "\\\"") <*> go
    
comma :: Parser ()
comma = skipSpace >> char ',' >> skipSpace

slash :: Parser ()
slash = skipSpace >> char '/' >> skipSpace

semicolon :: Parser ()
semicolon = skipSpace >> char ';' >> skipSpace

equal :: Parser ()
equal = skipSpace >> char '=' >> skipSpace

