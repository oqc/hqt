{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

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

, readQtfFiles
, readQpfFile

, defaultBrkToText
, qtfRngToQlf

, GrpStyle ( RefRangesOnly, BigBreaks, AllBreaks, ByVerse )
, applyGrpStyleToRng
) where

import           Prelude hiding ( readFile, lines, unlines )
import qualified System.IO as S ( readFile )
import qualified Data.List as S ( lines )
import qualified Data.Text as T
import           Data.Text ( Text, lines, pack )
import           Data.Text.IO
import           Data.Maybe ( listToMaybe )

import           Quran.Internal.Types


-- Type for a reference range, like: "80:1-8".
-- Single verse refs, like "31:6", are internally represented as "31:6-6".
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


-- Type for holding QTF and QPF files.
-- It has 6236 lines (could be: originals, translations, commentaries, break styles, etc.)
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


-- Read QTFs files
readQtfFiles :: [String] -> IO [QLines Text]  -- TODO: handle IO and parse exceptions
readQtfFiles files = mapM readFile files >>= mapM (qLines . lines) >>= return

-- Read a QPF file
readQpfFile :: String -> IO (QLines Int)
readQpfFile qpfFileName = do
  qpf <- S.readFile qpfFileName  -- TODO: handle exceptions, like "no valid qpf file" or "file non existant"
  qLines $ map (\s -> readNumOrZero [head s]) (S.lines qpf) >>= return
  where
    readNumOrZero :: String -> Int
    readNumOrZero s = case (fmap fst . listToMaybe . reads) s of Just i -> i; Nothing -> 0


-- The default mapping from QPF numbers to QLF style breaks
defaultBrkToText :: Int -> Text
defaultBrkToText brk = case brk of 0 -> " "
                                   1 -> "\\br "
                                   2 -> "\\bbr "
                                   _ -> "\\bbr "

-- Enrich a QTF text with refs and QPF-specified paragraphing into a QLF (LaTeX'ish) output
qtfRngToQlf :: QLines Int -> (Int -> Text) -> QLines Text -> QRefRng -> Text
qtfRngToQlf qpf brkToText qtf refRng = T.concat $
  weave3 (map (\rng -> pack $ "\\nr{" ++ show rng ++ "} ") $ splitRngByVerses refRng)
         (fromQLines qtf refRng)
         (map (brkToText . head . fromQLines qpf) $ (init . splitRngByVerses) refRng)
  where
    weave3 :: [a] -> [a] -> [a] -> [a]
    weave3 []     _      _      = []  -- ys and zs are woven into xs, so empty
    weave3 xs     []     _      = xs  -- zs needs ys to be woven into xs
    weave3 (x:xs) (y:ys) []     = x:y   : weave3 xs ys []
    weave3 (x:xs) (y:ys) (z:zs) = x:y:z : weave3 xs ys zs


-- Intra-text grouping style (what to align in case of side-by-side)
data GrpStyle = RefRangesOnly  -- only group on ranges (least grouping)
              | BigBreaks      -- on ranges and big breaks
              | AllBreaks      -- on ranges and all breaks
              | ByVerse        -- on individual verses (most grouping)
                deriving Show

-- Break up a QRefRng into [QRefRng] based on a grouping style and QPF
applyGrpStyleToRng :: GrpStyle -> QLines Int -> QRefRng -> [QRefRng]
applyGrpStyleToRng grpStyle qpf refRng = case grpStyle of
  RefRangesOnly -> [refRng]
  BigBreaks     -> splitRngByPars (1 <) qpf refRng
  AllBreaks     -> splitRngByPars (0 <) qpf refRng
  ByVerse       -> splitRngByVerses refRng


