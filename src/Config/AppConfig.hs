module Config.AppConfig (AppConfig (..), getConfig) where

import Config.CliConfig (CliConfig (..))
import Config.Files (ConfigDirectory)
import Config.YamlConfig (FintsConfig, LedgerConfig, YamlConfig (..), getYamlConfig)
import Data.Time (Day)

data AppConfig = Config
  { fintsConfig :: FintsConfig
  , ledgerConfig :: LedgerConfig
  , -- path where fints2ledger stores it's config files
    configDirectory :: ConfigDirectory
  , journalFile :: FilePath
  , -- the start date to pull the FinTS entries from (format YYYY/MM/DD)
    startDate :: Day
  , isDemo :: Bool
  , pythonExecutable :: String
  }
  deriving (Show)

getConfig :: CliConfig -> IO AppConfig
getConfig cliConfig = do
  yamlConfig <- getYamlConfig cliConfig.configDirectory
  return $
    Config
      { fintsConfig = yamlConfig.fints
      , ledgerConfig = yamlConfig.ledger
      , configDirectory = cliConfig.configDirectory
      , journalFile = cliConfig.journalFile
      , startDate = cliConfig.startDate
      , isDemo = cliConfig.isDemo
      , pythonExecutable = cliConfig.pythonExecutable
      }
