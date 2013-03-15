{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Quran.QLines (
  QLines
, qLines
, qLines_
, unQLines
, fromQLines

, readQtfFiles
, readQpfFiles

, defaultBrkToText
, qtfRngToQlf

, GrpStyle (RefRangesOnly, BigBreaks, AllBreaks, ByVerse)
, applyGrpStyleToRng

, splitRngByVerses
, splitRngByPars
) where


import           Prelude hiding (readFile, lines, unlines)
import qualified System.IO as S (readFile)
import qualified Data.List as S (lines)
import qualified Data.Text as T
import           Data.Text (Text, lines, pack)
import           Data.Text.IO
import           Data.Maybe (listToMaybe)

import           Quran.Internal.QRefRng
import           Quran.QRefRng


-- | Type for holding QTF and QPF files.
-- It has 6236 lines (could be: originals, translations, commentaries, break styles, etc.)
newtype QLines a = MkQLines [a]
  deriving (Show)

qLines :: Monad m => [a] -> m (QLines a)
qLines ls
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


-- | Read QTF files
readQtfFiles :: [FilePath] -> IO [QLines Text]  -- TODO: handle IO and parse exceptions
readQtfFiles files = mapM readFile files >>= mapM (qLines . lines) >>= return

-- | Read QPF files
readQpfFiles :: [FilePath] -> IO [QLines Int]
readQpfFiles files = mapM S.readFile files >>= mapM rawQpfToQLines >>= return
  where
    rawQpfToQLines :: Monad m => String -> m (QLines Int)
    rawQpfToQLines = qLines . (\qpf -> map (\s -> readNumOrZero [head s]) (S.lines qpf))
    readNumOrZero :: String -> Int
    readNumOrZero s = case (fmap fst . listToMaybe . reads) s of Just i -> i; Nothing -> 0


-- | The default mapping from QPF numbers to QLF style breaks
defaultBrkToText :: Int -> Text
defaultBrkToText brk = case brk of 0 -> " "
                                   1 -> "\\br "
                                   2 -> "\\bbr "
                                   _ -> "\\bbr "

-- | Enrich a QTF text with refs and QPF-specified paragraphing into a QLF (LaTeX'ish) output
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


-- | Intra-text grouping style (what to align in case of side-by-side)
data GrpStyle = RefRangesOnly  -- only group on ranges (least grouping)
              | BigBreaks      -- on ranges and big breaks
              | AllBreaks      -- on ranges and all breaks
              | ByVerse        -- on individual verses (most grouping)
                deriving Show

-- | Break up a `QRefRng` into `[QRefRng]` based on a grouping style and QPF
applyGrpStyleToRng :: GrpStyle -> QLines Int -> QRefRng -> [QRefRng]
applyGrpStyleToRng grpStyle qpf refRng = case grpStyle of
  RefRangesOnly -> [refRng]
  BigBreaks     -> splitRngByPars (1 <) qpf refRng
  AllBreaks     -> splitRngByPars (0 <) qpf refRng
  ByVerse       -> splitRngByVerses refRng


-- | Flattens a reference to a verse-range-free list of reference
splitRngByVerses :: QRefRng -> [QRefRng]
splitRngByVerses rr = map (\v -> MkQRefRng (chapter rr, (v, v))) $ verseList rr

-- | Splits a reference on a QPF paragraphing style using a `criterion`
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

