module Config.StartupChecks (runStartupChecks) where

import Config.CliConfig (CliConfig (..))
import Config.Files (ConfigDirectory (..), getConfigFilePath)
import Config.YamlConfig (YamlConfig (..), defaultYamlConfig, writeYamlConfig)
import Control.Monad (unless)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import UI.ConfigUI (runConfigUI)
import Utils ((??))

runStartupChecks :: CliConfig -> IO ()
runStartupChecks cliConfig = do
  createDirectoryIfMissing True cliConfig.configDirectory.get

  let configFilePath = getConfigFilePath cliConfig.configDirectory
  configFileExists <- doesFileExist configFilePath
  unless configFileExists do
    maybeConfig <- runConfigUI defaultYamlConfig cliConfig.configDirectory
    writeYamlConfig configFilePath defaultYamlConfig{fints = maybeConfig ?? defaultYamlConfig.fints}
