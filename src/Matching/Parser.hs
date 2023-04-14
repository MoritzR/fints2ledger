module Matching.Parser (amountParser, runParser) where

import Control.Applicative (optional)
import Data.Char (isDigit)
import Data.Maybe (listToMaybe)
import Text.ParserCombinators.ReadP (ReadP, char, choice, munch1, pfail, readP_to_S, string)
import Text.Read (readMaybe)
import Utils ((??))

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
  number <- decimal
  return (`comparison` number)

space :: ReadP Char
space = char ' '

-- TODO test
decimal :: ReadP Double
decimal = do
  beforeDot <- munch1 isDigit
  afterDot <- optional do
    char '.'
    munch1 isDigit
  let parsedNumber = readMaybe $ beforeDot ++ "." ++ (afterDot ?? "")
  maybe pfail return parsedNumber
