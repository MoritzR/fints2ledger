module UtilsSpec (spec) where

import Test.Syd (Spec, describe, it, shouldBe)
import Utils (formatDouble)

spec :: Spec
spec = do
  describe "showDouble" do
    it "formats floating point numbers with decimal places" do
      formatDouble 29.34 `shouldBe` "29.34"
    it "formats integers without decimal places" do
      formatDouble 29.0 `shouldBe` "29"
    it "doesn't use e notaion for smaller numbers" do
      formatDouble 0.01 `shouldBe` "0.01"
