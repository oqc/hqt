{-# LANGUAGE OverloadedStrings #-}

module QuranSpec.RefParser (refParserSpec) where

import Data.Attoparsec.Text (parseOnly)
import Test.Hspec

import Quran.Internal.QRefRng
import Quran.Internal.RefParser


refParserSpec :: Spec
refParserSpec = do

  describe "verseNr" $ do
    it "should parse series of digits" $
      testParser verseNr "12" (12, 12)
    it "should not parse digits when followed by a colon" $
      testParserFails verseNr "12:"

  describe "verseRng" $ do
    it "should parse a digit range" $
      testParser verseRng "1-2" (1, 2)
    it "should parse a digit range that is not spanning" $
      testParser verseRng "1-1" (1, 1)
    it "should not parse when the first value is missing" $
      testParserFails verseRng "-2"
    it "should not parse when the second value is missing" $
      testParserFails verseRng "1-"
    it "should not parse when only one value is given" $
      testParserFails verseRng "9"
    it "allows no spaces around the hyphen" $
      testParserFails verseRng "1 - 7"

  describe "verseNrOrRng" $ do
    it "should parse numbers" $
      testParser verseNrOrRng "12"  (12, 12)
    it "should parse ranges" $
      testParser verseNrOrRng "1-2" (1, 2)

  describe "verseNrsOrRngs" $ do
    it "should parse comma separated verse numbers" $
      testParser verseNrsOrRngs "1,2,3" [(1, 1), (2, 2), (3, 3)]
    it "should not parse a chapter number" $
      testParser verseNrsOrRngs "1,2,3:7" [(1, 1), (2, 2)]  -- the chapter is not in the result

  describe "refGrpByChapter" $ do
    it "should parse the simplest reference (1:1)" $
      testParser refRngByChapter "1:1"
        [qRefRng_ 1 (1, 1)]
    it "should parse a simple reference group" $
      testParser refRngByChapter "1:1,2,3"
        [qRefRng_ 1 (1, 1), qRefRng_ 1 (2, 2), qRefRng_ 1 (3, 3)]
    it "should parse a reference group with verse range" $
      testParser refRngByChapter "1:1,2,3-6,7"
        [qRefRng_ 1 (1, 1), qRefRng_ 1 (2, 2), qRefRng_ 1 (3, 6), qRefRng_ 1 (7, 7)]

  describe "refString" $ do
    it "should parse complex ref strings appropriately" $
      testParser refString "1:1,2,2:18,3:100-103"
        [qRefRng_ 1 (1, 1), qRefRng_ 1 (2, 2), qRefRng_ 2 (18, 18), qRefRng_ 3 (100, 103)]
    it "should not parse when whitespace is in" $
      testParserFails refString "1:1,2, 2:18, 3:100-103"

  where testParser p str success = case parseOnly p str of Left  _ -> False
                                                           Right x -> x == success
        testParserFails p str    = case parseOnly p str of Left  _ -> True
                                                           Right _ -> False

