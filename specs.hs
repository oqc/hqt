import QuranSpec.RefParser
import QuranSpec.QLines
import QuranSpec.QRefRng

import Test.Hspec (hspec)


main :: IO ()
main = hspec $ do
  refParserSpec
  qLinesSpec
  qRefRngSpec

