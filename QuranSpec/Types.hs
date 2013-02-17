module QuranSpec.Types ( typeSpec ) where

import Quran.Internal.Types
import Quran.Types
import Test.Hspec


typeSpec :: Spec
typeSpec = do

  describe "qRefRngToLineNr" $ do
    it "convert references to line numbers" $
      -- refToLineNr (114, 6) == 6236
      let refEqualsLineNr (r, lineNr) = qRefRngToLineNr r == lineNr
      in foldr (&&) True $ map refEqualsLineNr $
        [ (qRefRng_ 1   (1,   1),     1)
        , (qRefRng_ 2   (1,   1),     8)
        , (qRefRng_ 3   (1,   1),   294)
        , (qRefRng_ 3   (200, 1),   493)
        , (qRefRng_ 31  (6,   1),  3475)
        , (qRefRng_ 114 (6,   1),  6236)
        ]

  describe "qRefRng" $ do it "" $ pending
  describe "qRefRngToLineNrs" $ do it "" $ pending
  describe "splitRngByVerses" $ do it "" $ pending

  describe "splitRngByPars" $ do
    it "splits a ref range into smaller ranges as per paragraphing strategy" $
      foldr (&&) True $ map (\(crit, qpf, refRng, success) -> success == splitRngByPars crit qpf refRng) [
          ((0 <), qLines_ (take 6236 [0, 0..]), qRefRng_ 1 (1, 3), [qRefRng_ 1 (1, 3)])
        , ((0 <), qLines_ (take 6236 [1, 1..]), qRefRng_ 1 (1, 3), [qRefRng_ 1 (1, 1), qRefRng_ 1 (2, 2), qRefRng_ 1 (3, 3)])
        , ((0 <), qLines_ (take 6236 [1, 1..]), qRefRng_ 1 (5, 7), [qRefRng_ 1 (5, 5), qRefRng_ 1 (6, 6), qRefRng_ 1 (7, 7)])
        , ((1 <), qLines_ (take 6236 [2, 2..]), qRefRng_ 2 (5, 7), [qRefRng_ 2 (5, 5), qRefRng_ 2 (6, 6), qRefRng_ 2 (7, 7)])
        , ((0 <), qLines_ (take 6235 [i | i <- [0, 1, 2]]), qRefRng_ 1 (1, 3), [qRefRng_ 1 (1, 2), qRefRng_ 1 (3, 3)])
        ]

      {- foldr (&&) True $ map (\(crit, qpf, refRng, success) -> (show $ splitRngByPars crit qpf refRng) == success) [-}
      {-     ((0 <), qLines_ (take 6236 [0, 0..]), qRefRng_ 1 (1, 3), "[1:1-3]")-}
      {-   , ((0 <), qLines_ (take 6236 [1, 1..]), qRefRng_ 1 (1, 3), "[1:1-1,1:2-2,1:3-3]")-}
      {-   , ((0 <), qLines_ (take 6236 [1, 1..]), qRefRng_ 1 (5, 7), "[1:5-5,1:6-6,1:7-7]")-}
      {-   , ((1 <), qLines_ (take 6236 [2, 2..]), qRefRng_ 2 (5, 7), "[2:5-5,2:6-6,2:7-7]")-}
      {-   , ((0 <), qLines_ ([0] ++ take 6235 [1, 1..]), qRefRng_ 1 (1, 3), "[1:1-2,1:3-3]")-}
      {-   ]-}

  describe "qLines" $ do it "" $ pending

  describe "chapterOffset" $ do
    it "should sum up the verses of all chapters till 'c'" $
      foldr (&&) True $ map (\(c, success) -> success == chapterOffset c) [
          (0,   0)
        , (1,   0)
        , (2,   7)
        , (3, 293)
        ]

  describe "versesPerChapter" $ do
    it "should contain 114 + 1 elements"                 $ length versesPerChapter == 114 + 1
    it "expects the 9th chapter to have 129 verses"      $ versesPerChapter !! 9   == 129
    it "expects the 0th chapter to have 0 verses"        $ versesPerChapter !! 0   == 0
    it "expects the last chapter (114) to have 6 verses" $ versesPerChapter !! 114 == 6
    it "expects the total of verses to be 6236"          $ sum versesPerChapter    == 6236


