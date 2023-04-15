module UtilsSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy.IO as TLIO
import Paths_hsfints2ledger (getDataFileName)
import Test.Syd (Spec, describe, it, shouldBe)
import Transactions (Amount (Amount), Transaction (..))
import Utils (formatDouble)

spec :: Spec
spec = do
  describe "showDouble" do
    it "formats floating point numbers with decimal places" do
      formatDouble 29.34 `shouldBe` "29.34"
    it "formats integers without decimal places" do
      formatDouble 29.0 `shouldBe` "29"