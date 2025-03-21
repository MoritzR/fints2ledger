module Dates.Parser (parseDate) where

import Control.Applicative ((<|>))
import Data.Char (isNumber)
import Data.Maybe (listToMaybe)
import Data.Time (addDays, defaultTimeLocale, readPTime)
import Data.Time.Calendar (Day)
import Text.ParserCombinators.ReadP (
  ReadP,
  eof,
  many1,
  optional,
  readP_to_S,
  satisfy,
  string,
 )

parseDate :: Day -> String -> Either String Day
parseDate currentDay toParse = case listToMaybe . map fst . readP_to_S (dateParser currentDay) $ toParse of
  Just theDay -> Right theDay
  Nothing -> Left "Failed to parse a date. Valid examples are '31.12.2024' or '90 days ago'"

dateParser currentDay = parseAbsoluteDate <|> parseAgo currentDay

parseAbsoluteDate = readPTime True defaultTimeLocale "%d.%m.%Y" <* eof

parseAgo currentDay = do
  n <- number
  string " day"
  optional $ string "s"
  string " ago"
  eof
  return $ addDays (toInteger $ negate n) currentDay

number :: ReadP Int
number = do
  numberAsString <- many1 $ satisfy isNumber
  return $ read numberAsString
