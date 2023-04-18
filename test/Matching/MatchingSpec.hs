module Matching.MatchingSpec (spec) where

import Config.YamlConfig (Filling (Filling, match), fill)
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
