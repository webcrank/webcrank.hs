{-# LANGUAGE OverloadedStrings #-}

module Webcrank.Parsers
  ( parseAcceptHeader
  , parseMediaType
  , parseConnegHeader 
  , parseEtags
  ) where

import Control.Applicative (Applicative, (<|>), (<$>), (<*>), (<*), (*>), many)
import Control.Arrow (first)
import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly, sepBy, sepBy1, takeWhile1, char, (.*>), option, double, skipSpace, scan)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.List (sortBy)
import Data.Ord (comparing)

import Webcrank.Types.MediaType

-- | Given the value of an @Accept@ header, produce an ordered list
-- based on the q-values, with the head of the list being the 
-- highest-priority requested type.
parseAcceptHeader :: ByteString -> [MediaType]
parseAcceptHeader = order . parse where
  order = (snd <$>) . sortq . (prioritizeMediaType <$>) 
  sortq = sortBy (comparing (Down . fst))
  parse = either (const []) id . parseOnly acceptHeaderP
  acceptHeaderP = mediaTypeP `sepBy1` comma

prioritizeMediaType :: MediaType -> (Double, MediaType)
prioritizeMediaType (MediaType pri sub ps) = prioritize ps [] where
  prioritize [] ps' = (1, MediaType pri sub ps')
  prioritize (p@(k, v) : ps') ps'' | k == "q"  = (parseQ v, MediaType pri sub (ps' ++ ps''))
                                   | otherwise = prioritize ps' (p : ps'')
  parseQ "1" = 1
  parseQ v = either (const 1) id (parseOnly double v)

newtype Down a = Down a deriving (Eq)

instance Ord a => Ord (Down a) where
  compare (Down x) (Down y) = y `compare` x

parseMediaType :: ByteString -> Maybe MediaType
parseMediaType = either (const Nothing) Just . parseOnly mediaTypeP

mediaTypeP :: Parser MediaType
mediaTypeP = do
  (pri, sub) <- mediaRange
  ps         <- many (semicolon >> param)
  return $ MediaType (CI.mk pri) (CI.mk sub) (first CI.mk <$> ps)
  where mediaRange = skipSpace *> (wildcard <|> wildSub <|> mtype) <* skipSpace
        wildcard  = char '*' >> slash >> char '*' >> return ("*", "*")
        wildSub   = (\p -> (p, "*")) <$> token <* (slash >> char '*')
        mtype     = (,) <$> token <*> (slash *> token)
        param     = (,) <$> token <*> (equal *> paramVal)
        paramVal  = token <|> quotedString

parseConnegHeader :: ByteString -> [(Double, CI ByteString)]
parseConnegHeader = either (const []) id . parseOnly connegHeader where
  connegHeader = connegChoice `sepBy1` comma
  connegChoice = do
    accept <- token
    skipSpace
    q      <- option 1.0 (";" .*> ("q" .*> ("=" .*> double)))
    return (q, CI.mk accept)

parseEtags :: ByteString -> [ByteString]
parseEtags = either (const []) id . parseOnly etags where
  etags = etag `sepBy` comma
  etag = "W/" .*> quotedString <|> quotedString

token :: Parser ByteString
token = takeWhile1 (`notElem` cs) where
  cs = ['\0'..'\31'] ++ "()<>@,;:\\\"/[]?={} \t" ++ ['\128'..'\255']

quotedString :: Parser ByteString
quotedString = do
  _ <- char '"' 
  let scnr True _ = Just False
      scnr _  '"' = Nothing
      scnr _ c    = Just $ c == '\\'
  s <- scan False scnr
  _ <- char '"'
  return (if '\\' `B.elem` s then unescape s else s)

-- TODO improve efficiency with blaze builder?
unescape :: ByteString -> ByteString
unescape = B.concat . go
  where go s  = case B.elemIndex '\\' s of
                  Nothing -> [s]
                  Just i  -> B.take i s : go' (B.drop (i+1) s)
        go' s = B.take 1 s : go (B.tail s)

comma :: Parser ()
comma = skipSpace >> char ',' >> skipSpace

slash :: Parser ()
slash = skipSpace >> char '/' >> skipSpace

semicolon :: Parser ()
semicolon = skipSpace >> char ';' >> skipSpace

equal :: Parser ()
equal = skipSpace >> char '=' >> skipSpace

