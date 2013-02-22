module Quran.Internal.RefParser (
  refString
, refRngByChapter
, verses
, verseNrOrRange
, verseNr
, verseRange
) where

import Control.Applicative
import Data.Attoparsec.Text

import Quran.Types


refString :: Parser [QRefRng]
refString = fmap concat $ refRngByChapter `sepBy1` (char ',') <* endOfInput

refRngByChapter :: Parser [QRefRng]
refRngByChapter = do c   <- many digit <* char ':'
                     vs  <- verses
                     rrs <- mapM (qRefRng $ read c) vs
                     pure $! rrs

verses :: Parser [(Int, Int)]
verses = verseNrOrRange `sepBy1` (char ',')

verseNrOrRange, verseNr, verseRange :: Parser (Int, Int)
verseNrOrRange = (try verseNr) <|> (try verseRange)
verseNr = (\v -> (read v, read v)) <$> many1 digit <* (endOfInput <|> nextIsComma)
  where nextIsComma = do p <- peekChar
                         case p of Nothing -> empty
                                   Just x  -> if x == ',' then pure () else empty
verseRange = (\v1 v2 -> (read v1, read v2)) <$> (many1 digit <* char '-') <*> many1 digit

