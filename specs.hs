import QuranSpec.Types
import QuranSpec.RefParser

import Test.Hspec ( hspec )


main :: IO ()
main = hspec $ do
  refParserSpec
  typeSpec

