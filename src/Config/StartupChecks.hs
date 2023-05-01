module Config.StartupChecks (runStartupChecks) where

import Config.CliConfig (CliConfig (..))
import Config.Files (ConfigDirectory (..), getConfigFilePath)
import Config.YamlConfig (YamlConfig (..), defaultYamlConfig)
import Control.Monad (unless)
import Data.Yaml qualified as Yaml
import System.Directory (createDirectoryIfMissing, doesFileExist)
import UI.ConfigUI (runConfigUI)

runStartupChecks :: CliConfig -> IO ()
runStartupChecks cliConfig = do
  createDirectoryIfMissing True cliConfig.configDirectory.get

  let configFilePath = getConfigFilePath cliConfig.configDirectory
  configFileExists <- doesFileExist configFilePath
  unless configFileExists do
    fintsConfig <- runConfigUI defaultYamlConfig cliConfig.configDirectory
    writeYamlConfig configFilePath defaultYamlConfig{fints = fintsConfig}

writeYamlConfig :: FilePath -> YamlConfig -> IO ()
writeYamlConfig = Yaml.encodeFile
