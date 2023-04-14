module ConfigSpec (spec) where

import Config.YamlConfig (defaultYamlConfig)
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