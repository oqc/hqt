{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Quran.QRefRng (
  QRefRng
, qRefRng
, unQRefRng
, chapter
, fstVerse
, lstVerse
, verseList
, qRefRngToLineNr
, qRefRngToLineNrs
) where


import Quran.Internal.QRefRng


-- | `QRefRng` constructor that does checks reference validity
qRefRng :: Monad m => Int -> (Int, Int) -> m QRefRng
qRefRng c (v1, v2)
  | c  < 1 || c  > 114 ||
    v1 < 1 || v1 > versesPerChapter !! c ||
    v2 < 1 || v2 > versesPerChapter !! c =
                fail $ "Reference inexistant: " ++ (show $ MkQRefRng (c, (v1, v2)))
  | v1 > v2   = fail $ "Start verse cannot be beyond end verse in reference: " ++ (show $ MkQRefRng (c, (v1, v2)))
  | otherwise = return $ MkQRefRng (c, (v1, v2))

unQRefRng :: QRefRng -> (Int, (Int, Int))
unQRefRng (MkQRefRng (c, (v1, v2))) = (c, (v1, v2))

chapter :: QRefRng -> Int
chapter (MkQRefRng (c, _)) = c

fstVerse :: QRefRng -> Int
fstVerse (MkQRefRng (_, (v1, _))) = v1

lstVerse :: QRefRng -> Int
lstVerse (MkQRefRng (_, (_, v2))) = v2

verseList :: QRefRng -> [Int]
verseList (MkQRefRng (_, (v1, v2))) = [v1..v2]

-- | Get the line number of a reference (according to the QTF-style)
qRefRngToLineNr :: QRefRng -> Int
qRefRngToLineNr (MkQRefRng (c, (v1, _))) = v1 + chapterOffset c

-- | Get the line numbers of a list of references (according to the QTF-style)
qRefRngToLineNrs :: QRefRng -> [Int]
qRefRngToLineNrs (MkQRefRng (c, (v1, v2))) = map (chapterOffset c +) [v1..v2]

