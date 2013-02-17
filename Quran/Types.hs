{-# LANGUAGE FlexibleInstances #-}

module Quran.Types (
  QRefRng
, qRefRng
, qRefRng_
, unQRefRng
, chapter
, fstVerse
, lstVerse
, verseList
, qRefRngToLineNr
, qRefRngToLineNrs
, splitRngByVerses
, splitRngByPars

, QLines
, qLines
, qLines_
, unQLines
, fromQLines
) where

import Quran.Internal.Types


-- For reference ranges like: "80:1-8"
newtype QRefRng = MkQRefRng (Int, (Int, Int))
  deriving ( Eq, Ord )

instance Show QRefRng where
  show (MkQRefRng (c, (v1, v2))) = show c ++ ":" ++ show v1 ++ (if v1 == v2 then  ""  else  "-" ++ show v2)

qRefRng :: Monad m => Int -> (Int, Int) -> m QRefRng
qRefRng c (v1, v2)  -- Programming errors here are no substitute for parser exceptions
  | c < 1 || c > 114 || v1 < 1 || v2 > versesPerChapter !! c =
                fail $ "Reference inexistant: " ++ (show $ MkQRefRng (c, (v1, v2)))
  | v1 > v2   = fail $ "Start-verse cannot be larger than end-verse in reference: " ++ (show $ MkQRefRng (c, (v1, v2)))
  | otherwise = return $ MkQRefRng (c, (v1, v2))

qRefRng_ :: Int -> (Int, Int) -> QRefRng
qRefRng_ c vs = MkQRefRng (c, vs)

unQRefRng :: QRefRng -> (Int, (Int, Int))
unQRefRng (MkQRefRng (c, (v1, v2))) = (c, (v1, v2))

chapter ::QRefRng -> Int
chapter (MkQRefRng (c, _)) = c

fstVerse :: QRefRng -> Int
fstVerse (MkQRefRng (_, (v1, _))) = v1

lstVerse :: QRefRng -> Int
lstVerse (MkQRefRng (_, (_, v2))) = v2

verseList :: QRefRng -> [Int]
verseList (MkQRefRng (_, (v1, v2))) = [v1..v2]

qRefRngToLineNr  :: QRefRng -> Int
qRefRngToLineNr  (MkQRefRng (c, (v1, _)))  = v1 + chapterOffset c

qRefRngToLineNrs :: QRefRng -> [Int]
qRefRngToLineNrs (MkQRefRng (c, (v1, v2))) = map (chapterOffset c +) [v1..v2]

splitRngByVerses :: QRefRng -> [QRefRng]
splitRngByVerses rr = map (\v -> MkQRefRng (chapter rr, (v, v))) $ verseList rr

splitRngByPars :: (Int -> Bool) -> QLines Int -> QRefRng -> [QRefRng]
splitRngByPars criterion qpf refRng =
  let markedPairs = zip (map criterion $ fromQLines qpf refRng)
                        (map (\x -> [x, x + 1]) $ verseList refRng)
  in map (\vs -> qRefRng_ (chapter refRng) vs)
         (listToTuples ([fstVerse refRng] ++ (concat . (map snd) . (filter fst) . init $ markedPairs) ++ [lstVerse refRng]))
  where
    listToTuples :: [a] -> [(a, a)]
    listToTuples []       = []
    listToTuples [_]      = error "Odd lists should never occur"
    listToTuples (x:y:rs) = (x, y) : listToTuples rs


-- Any kind of resource that has 6236 lines (originals, translations, commentaries, break styles, etc.)
newtype QLines a = MkQLines [a]
  deriving ( Show )

qLines :: Monad m => [a] -> m (QLines a)
qLines ls  -- Programming errors here are no substitute for parser exceptions
  | length ls /= 6236 = fail $ "The QTF format expects 6236 lines."
  | otherwise         = return $ MkQLines ls

qLines_ :: [a] -> QLines a
qLines_ ls = MkQLines ls

unQLines :: QLines a -> [a]
unQLines (MkQLines ts) = ts

class QLinesSelector s where
  fromQLines :: QLines a -> s -> [a]
instance QLinesSelector Int     where fromQLines (MkQLines ls) n  = [ls !! (n - 1)]
instance QLinesSelector [Int]   where fromQLines (MkQLines ls) ns = map ((!!) ls . subtract 1) ns
instance QLinesSelector QRefRng where fromQLines qls           rr = fromQLines qls $ qRefRngToLineNrs rr

