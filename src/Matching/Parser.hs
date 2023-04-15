module Matching.Parser (amountParser, runParser, endOnDecimal) where

import Control.Applicative (optional)
import Data.Maybe (listToMaybe)
import Text.ParserCombinators.ReadP (ReadP, char, choice, munch1, pfail, readP_to_S, string)
import Text.Read (readMaybe)

runParser :: ReadP a -> String -> Maybe a
runParser parser = listToMaybe . map fst . readP_to_S parser

comparisons :: [(String, Double -> Double -> Bool)]
comparisons =
  [ ("<", (<))
  , ("<=", (<=))
  , ("=", (==))
  , ("", (==))
  , (">", (>))
  , (">=", (>=))
  ]

amountParser :: ReadP (Double -> Bool)
amountParser = do
  comparison <- choice $ map (\(s, f) -> f <$ string s) comparisons
  optional space
  number <- endOnDecimal
  return (`comparison` number)

space :: ReadP Char
space = char ' '

endOnDecimal :: ReadP Double
endOnDecimal = do
  allRemainingCharacters <- munch1 $ const True
  maybe pfail return (readMaybe allRemainingCharacters)
