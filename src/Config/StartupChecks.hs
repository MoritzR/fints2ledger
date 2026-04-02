{-# LANGUAGE OverloadedLabels #-}

module Config.StartupChecks (runStartupChecks) where

import Config.Banks (fintsEndpoint)
import Config.CliConfig (CliConfig (..))
import Config.Files (ConfigDirectory (..), getConfigFilePath)
import Config.YamlConfig (defaultYamlConfig, writeYamlConfig)
import Control.Exception (Exception, throwIO)
import Control.Lens ((.~))
import Control.Monad (unless)
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Text (pack)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import UI.BankSelectionUI (runBankSelectionUI)
import UI.ConfigUI (runConfigUI)

newtype ConfigSetupAborted = ConfigSetupAborted String deriving (Show)

instance Exception ConfigSetupAborted

runStartupChecks :: CliConfig -> IO ()
runStartupChecks cliConfig = do
  createDirectoryIfMissing True cliConfig.configDirectory.get

  let configFilePath = getConfigFilePath cliConfig.configDirectory
  configFileExists <- doesFileExist configFilePath
  unless configFileExists do
    maybeBank <- runBankSelectionUI
    case maybeBank of
      Nothing -> throwIO $ ConfigSetupAborted "Please select a bank before continuing."
      Just bank -> do
        let initialConfig = defaultYamlConfig & #fints . #endpoint .~ pack (fintsEndpoint bank)
        maybeConfig <- runConfigUI initialConfig cliConfig.configDirectory
        case maybeConfig of
          Just config -> writeYamlConfig configFilePath config
          Nothing -> throwIO $ ConfigSetupAborted "Please fill out and save the config form before continuing."
