module Quran.RefParser (
  parseRefRngs
) where

import Data.Attoparsec.Text ( parseOnly )
import Data.Text ( Text )

import Quran.Internal.RefParser
import Quran.QRefRng (QRefRng)


parseRefRngs :: Text -> Either String [QRefRng]
parseRefRngs = parseOnly refString

