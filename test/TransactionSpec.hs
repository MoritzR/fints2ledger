module TransactionSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy.IO as TLIO
import Paths_hsfints2ledger (getDataFileName)
import Test.Syd (Spec, describe, it, shouldBe)
import Transactions (Amount (Amount), Transaction (..))

spec :: Spec
spec = do
  describe "Transactions" do
    it "parses the sample transactions" do
      sampleTransaction <- TLIO.readFile =<< getDataFileName "data/example.json"
      let parsedTransactions = Aeson.eitherDecode (encodeUtf8 sampleTransaction)

      head <$> parsedTransactions
        `shouldBe` Right
          Transaction
            { date = "2022/03/23",
              amount = Amount (-19.69),
              currency = "EUR",
              posting = "Lastschrifteinzug",
              payee = "VISA KAUFLAND",
              purpose = "NR XXXX 1234 KAUFUMSATZ120092309482309480239"
            }