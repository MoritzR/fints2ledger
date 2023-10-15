module TransactionSpec (spec) where

import Config.Files (exampleFile)
import Data.Aeson qualified as Aeson
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Test.Syd (Spec, describe, it, shouldBe, shouldContain)
import Transactions (Amount (Amount), Transaction (..), transactionsToCsv)
import Utils (byteStringToString)

spec :: Spec
spec = do
  describe "Transactions" do
    it "parses the sample transactions" do
      let parsedTransactions = Aeson.eitherDecode (encodeUtf8 $ fromStrict exampleFile)

      head <$> parsedTransactions
        `shouldBe` Right
          Transaction
            { date = "2022/03/23"
            , amount = Amount (-19.69)
            , currency = "EUR"
            , posting = "Lastschrifteinzug"
            , payee = "VISA KAUFLAND"
            , purpose = "NR XXXX 1234 KAUFUMSATZ120092309482309480239"
            }

    it "writes 0.01 to csv (instead of 1.0e-2)" do
      let transaction =
            Transaction
              { date = "2022/03/23"
              , amount = Amount 0.01
              , currency = "EUR"
              , posting = "Lastschrifteinzug"
              , payee = "VISA KAUFLAND"
              , purpose = "NR XXXX 1234 KAUFUMSATZ120092309482309480239"
              }

      byteStringToString (transactionsToCsv [transaction]) `shouldContain` "0.01"
