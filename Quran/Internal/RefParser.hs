module Quran.Internal.RefParser (
  refString
, refRngByChapter
, verses
, verseNrOrRange
, verseNr
, verseRange
) where

import Text.ParserCombinators.Parsec
import Quran.Types


refString :: GenParser Char st [QRefRng]
refString = do refRngs <- many refRngByChapter
               eof
               return $ concat refRngs

refRngByChapter :: GenParser Char st [QRefRng]
refRngByChapter = do c   <- many digit
                     _   <- char ':'
                     vs  <- verses
                     rrs <- mapM (qRefRng $ read c) vs
                     return $ rrs

verses :: GenParser Char st [(Int, Int)]
verses = sepEndBy1 verseNrOrRange (spaces >> char ',' >> spaces) >>= return

verseNrOrRange, verseNr, verseRange :: GenParser Char st (Int, Int)
verseNrOrRange = do verseNr <|> verseRange
verseNr = try $ do n <- many1 digit
                   notFollowedBy $ oneOf ":-"
                   return (read n, read n)
verseRange = try $ do startNr <- many1 digit
                      (spaces >> char '-' >> spaces)
                      endNr <- many1 digit
                      return (read startNr, read endNr)

