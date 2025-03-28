module Config.StartupChecks (runStartupChecks) where

import Config.CliConfig (CliConfig (..))
import Config.Files (ConfigDirectory (..), getConfigFilePath)
import Config.YamlConfig (defaultYamlConfig, writeYamlConfig)
import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import UI.ConfigUI (runConfigUI)

newtype ConfigSetupAborted = ConfigSetupAborted String deriving (Show)

instance Exception ConfigSetupAborted

runStartupChecks :: CliConfig -> IO ()
runStartupChecks cliConfig = do
  createDirectoryIfMissing True cliConfig.configDirectory.get

  let configFilePath = getConfigFilePath cliConfig.configDirectory
  configFileExists <- doesFileExist configFilePath
  unless configFileExists do
    maybeConfig <- runConfigUI defaultYamlConfig cliConfig.configDirectory
    case maybeConfig of
      Just config -> writeYamlConfig configFilePath config
      Nothing -> throwIO $ ConfigSetupAborted "Please fill out and save the config form before continuing."
