module Config.StartupChecks (runStartupChecks) where

import Config.CliConfig (CliConfig (..))
import Config.Files (ConfigDirectory (..), getConfigFilePath, getTemplatePath)
import Config.YamlConfig (defaultYamlConfig)
import Control.Monad (unless)
import Data.Yaml qualified as Yaml
import Paths_hsfints2ledger
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)

runStartupChecks :: CliConfig -> IO ()
runStartupChecks cliConfig = do
  createDirectoryIfMissing True cliConfig.configDirectory.get

  let configFile = getConfigFilePath cliConfig.configDirectory
  configFileExists <- doesFileExist configFile
  unless configFileExists do
    putStrLn $ "I created '" <> configFile <> "' because it didn't exist yet. Go to that file to fill in missing values."
    Yaml.encodeFile configFile defaultYamlConfig

  let templateFile = getTemplatePath cliConfig.configDirectory
  templateFileExists <- doesFileExist templateFile
  unless templateFileExists do
    defaultTemplateFile <- getDataFileName "data/template.txt"
    copyFile defaultTemplateFile templateFile
