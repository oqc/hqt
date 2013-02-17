module QuranSpec.RefParser ( refParserSpec ) where


import Quran.Internal.RefParser
import Quran.Types

import Text.ParserCombinators.Parsec ( parse )

import Test.Hspec



refParserSpec :: Spec
refParserSpec = do

  describe "verseRange" $ do
    it "should parse a verse range to a [verseNr]" $
      testParser verseRange "1-7" (1, 7)
    it "allows spaces around the hyphen" $
      testParser verseRange "1 - 7" (1, 7)

  describe "verseNr" $ do
    it "should parse series of digits" $
      testParser verseNr "12" (12, 12)
    it "should not parse digits when followed by a colon" $
      testParserFails verseNr "123:"

  describe "verses" $ do
    it "should parse comma separated verse numbers" $
      testParser verses "1,2,3" [(1, 1), (2, 2), (3, 3)]
    it "should not parse a chapter number" $
      testParser verses "1,2,3:7" [(1, 1), (2, 2)]
    it "allows spaces arround the commas" $
      testParser verses "1, 2 ,3" [(1, 1), (2, 2), (3, 3)]

  describe "refGrpByChapter" $ do
    it "should parse a simple reference group appropriately" $
      testParser refRngByChapter "1:1,2,3"
        [qRefRng_ 1 (1, 1), qRefRng_ 1 (2, 2), qRefRng_ 1 (3, 3)]
    it "should parse a reference group with verse range appropriately" $
      testParser refRngByChapter "1:1,2,3-6,7"
        [qRefRng_ 1 (1, 1), qRefRng_ 1 (2, 2), qRefRng_ 1 (3, 6), qRefRng_ 1 (7, 7)]

  describe "refString" $ do
    it "should parse complex ref strings appropriately" $
      testParser refString "1:1,2, 2:18, 3:100-103"
        [qRefRng_ 1 (1, 1), qRefRng_ 1 (2, 2), qRefRng_ 2 (18, 18), qRefRng_ 3 (100, 103)]

  where testParser p str success = case parse p "" str of Left  _ -> False
                                                          Right x -> x == success
        testParserFails p str    = case parse p "" str of Left  _ -> True
                                                          Right _ -> False

