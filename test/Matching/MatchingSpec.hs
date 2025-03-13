module Matching.MatchingSpec (spec) where

import Config.YamlConfig (Filling (Filling, match, fill))
import Data.Map (empty, fromList)
import Matching.Matching (findMatch)
import Test.Syd (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "findMatch" do
    it "finds a match for regex (.*)" do
      let filling =
            Filling
              { match = fromList [("payee", ".*")]
              , fill = empty
              }
      let result =
            findMatch
              (fromList [("payee", "some payee")])
              [filling]

      result `shouldBe` Just filling

    it "finds a match for regex (VALUE)" do
      let filling =
            Filling
              { match = fromList [("payee", "VALUE")]
              , fill = empty
              }
      let result =
            findMatch
              (fromList [("payee", "VALUE")])
              [filling]

      result `shouldBe` Just filling

    it "finds no match when the regex doesn't match" do
      let filling =
            Filling
              { match = fromList [("payee", "regex")]
              , fill = empty
              }
      let result =
            findMatch
              (fromList [("payee", "no match")])
              [filling]

      result `shouldBe` Nothing

    it "finds a match for amount comparison (<=) when the amount is lower" do
      let filling =
            Filling
              { match = fromList [("amount", "<=10")]
              , fill = empty
              }
      let result =
            findMatch
              (fromList [("amount", "5.25")])
              [filling]

      result `shouldBe` Just filling

    it "finds no match for amount comparison (<=) when the amount is higher" do
      let filling =
            Filling
              { match = fromList [("amount", "<=10")]
              , fill = empty
              }
      let result =
            findMatch
              (fromList [("amount", "12")])
              [filling]

      result `shouldBe` Nothing

    it "finds a match when all of the regexes match" do
      let filling =
            Filling
              { match = fromList [("payee", ".*"), ("purpose", ".*")]
              , fill = empty
              }

      let result =
            findMatch
              (fromList [("payee", "a match"), ("purpose", "also a match")])
              [filling]

      result `shouldBe` Just filling

    it "finds no match when not all of the regexes match" do
      let filling =
            Filling
              { match = fromList [("payee", "regex"), ("purpose", ".*")]
              , fill = empty
              }
      let result =
            findMatch
              (fromList [("payee", "no match"), ("purpose", "a match")])
              [filling]

      result `shouldBe` Nothing

    it "returns only the first filling when two overlap" do
      let fillings =
            [ Filling
                { match = fromList [("payee", ".*")]
                , fill = fromList [("purpose", Just "first filling")]
                }
            , Filling
                { match = fromList [("payee", ".*")]
                , fill = fromList [("purpose", Just "second filling")]
                }
            ]

      let result =
            findMatch
              (fromList [("payee", "a match")])
              fillings

      result `shouldBe` Just (fillings !! 0)
