module QuranSpec.QLines ( qLinesSpec ) where

import Quran.Internal.QRefRng
import Quran.QLines
import Test.Hspec


qLinesSpec :: Spec
qLinesSpec = do

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


