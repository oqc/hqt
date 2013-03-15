module QuranSpec.QRefRng ( qRefRngSpec ) where

import Quran.Internal.QRefRng
import Quran.QRefRng
import Test.Hspec


qRefRngSpec :: Spec
qRefRngSpec = do

  -- SPECS FOR INTERNAL

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


  -- SPECS FOR PUBLIC

  describe "qRefRng" $ do
    it "should construct a QRefRng type when provided with valid input" $ pending
      {- qRefRng 31 (6, 6) == qRefRng_ 31 (6, 6)-}

    it "should fail when the chapter number is out of bounds" $ pending
    it "should fail when the end verse is smaller then the start verse" $ pending

  describe "qRefRngToLineNr" $ do
    it "converts references to line numbers" $
      -- refToLineNr (114, 6) == 6236
      let refEqualsLineNr (r, lineNr) = qRefRngToLineNr r == lineNr
      in foldr (&&) True $ map refEqualsLineNr $
        [ (qRefRng_ 1   (1,   1),     1)
        , (qRefRng_ 2   (1,   1),     8)
        , (qRefRng_ 3   (1,   1),   294)
        , (qRefRng_ 31  (6,   6),  3475)
        , (qRefRng_ 114 (6,   6),  6236)
        ]

  describe "qRefRngToLineNrs" $ do
    it "converts a reference range to a list of line numbers" $
      let refEqualsLineNrs (r, lineNr) = qRefRngToLineNrs r == lineNr
      in foldr (&&) True $ map refEqualsLineNrs $
        [ (qRefRng_ 1   (1,   1), [1])
        , (qRefRng_ 2   (1,   2), [8, 9])
        , (qRefRng_ 3   (1,   3), [294, 295, 296])
        , (qRefRng_ 114 (1,   6), [6231..6236])
        ]

