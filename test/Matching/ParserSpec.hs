module Matching.ParserSpec (spec) where

import Data.Foldable (forM_)
import Matching.Parser qualified as P
import Test.Syd (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Parsing amount matches" do
    let runParserWith number = P.runParser (fmap ($ number) P.amountParser)
    let tests =
          -- TODO test negative values
          [ ("<= 99.2", [80, 99.2], [99.3, 100])
          , ("< 99.2", [80], [99.2, 99.3, 100])
          , ("= 99.2", [99.2], [-100, 80, 99.3, 100])
          , ("99.2", [99.2], [-100, 80, 99.3, 100])
          , (">= 99.2", [99.2, 99.3, 100], [80])
          , ("> 99.2", [99.3, 100], [80, 99.2])
          ]
    forM_ tests \(toParse, examples, counterExamples) -> do
      describe ("parsing '" ++ toParse ++ "'") do
        forM_ examples \example -> do
          it ("accepts '" ++ show example ++ "'") do
            runParserWith example toParse `shouldBe` Just True
        forM_ counterExamples \example -> do
          it ("rejects '" ++ show example ++ "'") do
            runParserWith example toParse `shouldBe` Just False