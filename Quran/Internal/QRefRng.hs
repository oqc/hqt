module Quran.Internal.QRefRng (
  QRefRng (MkQRefRng)
, qRefRng_

, chapterOffset
, versesPerChapter
) where

-- | Type for a reference range, like: "80:1-8".
-- Single verse refs, like "31:6", are internally represented as "31:6-6".
newtype QRefRng = MkQRefRng (Int, (Int, Int))
  deriving (Eq, Ord)

-- | Prints reference ranges nicely, where a range of one verse is NOT printed as a range
instance Show QRefRng where
  show (MkQRefRng (c, (v1, v2))) = show c ++ ":" ++ show v1 ++ (if v1 == v2 then  ""  else  "-" ++ show v2)

-- | `QRefRng` constructor that does not check validity (but is pure)
qRefRng_ :: Int -> (Int, Int) -> QRefRng
qRefRng_ c vs = MkQRefRng (c, vs)


-- | The accumulation of all verses in chapters up untill chapter `c`
chapterOffset :: Int -> Int
chapterOffset c = sum $ take c versesPerChapter

-- | Array of the verses per chapter where index 0 a dummy chapter with zero verses
versesPerChapter :: [Int]
versesPerChapter = [
  -- Start with zero as the 0th chapter has no verses.
  -- Chapter 9 has 129 verses as per the QTF standard.
  0,   7,   286, 200, 176, 120, 165, 206, 75,  129,
  109, 123, 111, 43,  52,  99,  128, 111, 110, 98,
  135, 112, 78,  118, 64,  77,  227, 93,  88,  69,
  60,  34,  30,  73,  54,  45,  83,  182, 88,  75,
  85,  54,  53,  89,  59,  37,  35,  38,  29,  18,
  45,  60,  49,  62,  55,  78,  96,  29,  22,  24,
  13,  14,  11,  11,  18,  12,  12,  30,  52,  52,
  44,  28,  28,  20,  56,  40,  31,  50,  40,  46,
  42,  29,  19,  36,  25,  22,  17,  19,  26,  30,
  20,  15,  21,  11,  8,   8,   19,  5,   8,   8,
  11,  11,  8,   3,   9,   5,   4,   7,   3,   6,
  3,   5,   4,   5,   6 ]

