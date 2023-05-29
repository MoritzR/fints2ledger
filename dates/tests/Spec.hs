
import Test.Hspec
import Data.Dates

todayString :: String
todayString = "27 June 2019, 19:55:11"

main :: IO ()
main = hspec $ describe "test suite" $ do
  it "parses full absolute date specification" $ do
    let Right today = parseDateTime undefined todayString
    today `shouldBe` DateTime 2019 6 27 19 55 11

  it "parses simple absolute format #1" $ do
    let Right day = parseDate undefined "2019/06/27"
    day `shouldBe` DateTime 2019 6 27 0 0 0

  it "parses simple absolute format #2" $ do
    let Right day = parseDate undefined "27.06.2019"
    day `shouldBe` DateTime 2019 6 27 0 0 0

  it "parses absolute format #3" $ do
    let Right day = parseDate undefined "27 June 2019"
    day `shouldBe` DateTime 2019 6 27 0 0 0

  it "parses simple relative formats" $ do
    let Right today = parseDateTime undefined todayString
        Right today1 = parseDate today "today"
        Right yesterday = parseDate today "yesterday"
        Right tomorrow = parseDate today "tomorrow"

    today1 `shouldBe` DateTime 2019 6 27 19 55 11
    yesterday `shouldBe` DateTime 2019 6 26 19 55 11
    tomorrow `shouldBe` DateTime 2019 6 28 19 55 11

  it "parses more complex relative formats" $ do
    let Right today = parseDateTime undefined todayString
        Right inTwoDays = parseDate today "in 2 days"
        Right threeWeeksAgo = parseDate today "3 weeks ago"
        Right lastMonday = parseDate today "last monday"
        Right nextFriday = parseDate today "next friday"
        Right lastMonth = parseDate today "last month"
        Right nextYear = parseDate today "next year"

    inTwoDays `shouldBe` DateTime 2019 6 29 19 55 11
    threeWeeksAgo `shouldBe` DateTime 2019 6 6 19 55 11
    lastMonday `shouldBe` DateTime 2019 6 24 19 55 11
    nextFriday `shouldBe` DateTime 2019 6 28 19 55 11
    lastMonth `shouldBe` DateTime 2019 6 1 19 55 11
    nextYear `shouldBe` DateTime 2020 1 1 19 55 11

