module Matching.ParserSpec (spec) where

import Matching.Parser qualified as P
import Test.QuickCheck (property)
import Test.Syd (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "endOnDecimal" do
    let runParswerWith = P.runParser P.endOnDecimal
    it "parses all doubles that end a string" do
      property \(number :: Double) ->
        runParswerWith (show number) `shouldBe` Just number

  describe "Parsing amount matches" do
    let runParserWith number = P.runParser (fmap ($ number) P.amountParser)
    it "accepts numbers in that range" do
      runParserWith 80 "<90" `shouldBe` Just True
    it "rejects numbers outside of that range" do
      runParserWith 90.01 "<= 90" `shouldBe` Just False
    it "parses works with negative numbers" do
      runParserWith (-100) "<= -80" `shouldBe` Just True
      runParserWith 20.02 "> -80.99" `shouldBe` Just True
      runParserWith (-20.02) "<-80" `shouldBe` Just False