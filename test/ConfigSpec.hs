module ConfigSpec (spec) where

import Config.YamlConfig (LedgerConfig (..), YamlConfig (..), defaultYamlConfig, validateYamlConfig)
import Data.Text.Encoding as T
import Data.Text.IO as TIO
import Data.Yaml qualified as Yaml
import Test.Syd (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "YamlConfig" do
    it "encodes the default YamlConfig to the provided yaml file" do
      expectedYaml <- TIO.readFile "test/files/defaultConfig.yml"

      let encodedYaml = T.decodeUtf8 $ Yaml.encode defaultYamlConfig

      encodedYaml `shouldBe` expectedYaml

    it "encoding and then decoding results in the same config" do
      result <- Yaml.decodeThrow (Yaml.encode defaultYamlConfig)
      result `shouldBe` defaultYamlConfig

    it "returns an error when there are invalid md5 keys" do
      let validKeys = validateYamlConfig $ defaultYamlConfig{ledger = defaultYamlConfig.ledger{md5 = ["purpose"]}}
          hasInvalidKey = validateYamlConfig $ defaultYamlConfig{ledger = defaultYamlConfig.ledger{md5 = ["invalid", "purpose"]}}

      validKeys `shouldBe` Nothing
      hasInvalidKey
        `shouldBe` Just
          "md5 values are not valid: [\"invalid\"], only [\"amount\",\"currency\",\"date\",\"payee\",\"posting\",\"purpose\"] are allowed"
