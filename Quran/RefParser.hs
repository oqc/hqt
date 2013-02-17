module Quran.RefParser (
  parseRefRngs
) where

import Text.ParserCombinators.Parsec ( parse, ParseError )

import Quran.Types
import Quran.Internal.RefParser


parseRefRngs :: String -> Either ParseError [QRefRng]
parseRefRngs = parse refString ""

