module Quran.Internal.RefParser (
  refString
, refRngByChapter
, chapterNr
, verseNrsOrRngs
, verseNrOrRng
, verseNr
, verseRng
) where

import Control.Applicative ((<*), (<*>), (<$>), (<|>), many, empty, pure)
import Data.Attoparsec.Text

import Quran.QRefRng (QRefRng, qRefRng)


-- | Parses a string that contains one or more Quran references, like: "1:1-7,31:6"
refString :: Parser [QRefRng]
refString = fmap concat $ refRngByChapter `sepBy1` (char ',') <* endOfInput

-- | Parses a single Quran reference (`QRefRng` constructor with range checks)
refRngByChapter :: Parser [QRefRng]
refRngByChapter = do c <- chapterNr
                     (mapM (qRefRng c) =<< verseNrsOrRngs) >>= pure

-- | Parses a chapter number (always followed by a colon)
chapterNr :: Parser Int
chapterNr = read <$> (many digit <* char ':')

-- | Parses a one or more verse numbers or verse ranges
verseNrsOrRngs :: Parser [(Int, Int)]
verseNrsOrRngs = verseNrOrRng `sepBy1` (char ',')

-- Both verse numbers and verse ranges are internally represented by a tuple
verseNrOrRng, verseRng, verseNr :: Parser (Int, Int)
-- | Maybe parses a verse number, otherwise tries to parse a verse range
verseNrOrRng = (try verseNr) <|> (try verseRng)
-- | Parses a verse range (identified by the hyphen)
verseRng = (\v1 v2 -> (read v1, read v2)) <$> (many1 digit <* char '-')   <*> many1 digit
-- | Parses a verse number (always followed by a comma or `endOfInput`)
verseNr  = (\v     -> (read v,  read v))  <$>  many1 digit <* (endOfInput <|> nextIsComma)
  where nextIsComma = peekChar >>= isComma
        isComma p   = case p of Nothing -> empty
                                Just x  -> if x /= ',' then empty else pure ()

