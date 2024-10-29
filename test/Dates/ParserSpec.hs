module Dates.ParserSpec (spec) where

import Dates.Parser (parseDate)
import Test.Syd (Spec, describe, it, shouldBe, runIO)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Time.Calendar (Day)

spec :: Spec
spec = do
  describe "DatesParser when today is the '21.12.2024'" do
    today <- runIO $ parseDateFromTimeLibrary "21.12.2024"

    testDays today [
      ( "25.12.2024",  "25.12.2024"), 
      ( "01.01.1980",  "01.01.1980"), 
      ( "20 days ago", "01.12.2024"), 
      ( "1 day ago", "20.12.2024")]

parseDateFromTimeLibrary :: String -> IO Day
parseDateFromTimeLibrary = parseTimeM False defaultTimeLocale "%d.%m.%Y" 

testDays today = mapM_ \(toParse, expectedAsString) ->
  it ("parses '" ++ toParse ++ "' to '" ++ expectedAsString ++ "'") do
      let result = parseDate today toParse
      expected <- parseDateFromTimeLibrary expectedAsString

      result `shouldBe` Right expected
