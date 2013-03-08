module Quran.Internal.RefParser (
  refString
, refRngByChapter
, chapterNr
, verseNrsOrRngs
, verseNrOrRng
, verseNr
, verseRng
) where

import Control.Applicative
import Data.Attoparsec.Text

import Quran.Types hiding (chapter)


refString :: Parser [QRefRng]
refString = fmap concat $ refRngByChapter `sepBy1` (char ',') <* endOfInput

refRngByChapter :: Parser [QRefRng]
refRngByChapter = do c <- chapterNr
                     (mapM (qRefRng c) =<< verseNrsOrRngs) >>= pure

chapterNr      = read <$> (many digit <* char ':') :: Parser Int
verseNrsOrRngs = verseNrOrRng `sepBy1` (char ',')  :: Parser [(Int, Int)]

verseNrOrRng, verseRng, verseNr                    :: Parser (Int, Int)
verseNrOrRng = (try verseNr) <|> (try verseRng)
verseRng = (\v1 v2 -> (read v1, read v2)) <$> (many1 digit <* char '-')   <*> many1 digit
verseNr  = (\v     -> (read v,  read v))  <$>  many1 digit <* (endOfInput <|> nextIsComma)
  where nextIsComma = peekChar >>= isComma
        isComma p   = case p of Nothing -> empty
                                Just x  -> if x /= ',' then empty else pure ()

