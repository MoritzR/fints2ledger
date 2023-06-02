module Config.AppConfig (AppConfig (..), makeAppConfig) where

import Config.CliConfig (CliConfig (..))
import Config.Files (ConfigDirectory)
import Config.YamlConfig (FintsConfig, LedgerConfig, YamlConfig (..))
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

makeAppConfig :: CliConfig -> YamlConfig -> AppConfig
makeAppConfig cliConfig yamlConfig =
  Config
    { fintsConfig = yamlConfig.fints
    , ledgerConfig = yamlConfig.ledger
    , configDirectory = cliConfig.configDirectory
    , journalFile = cliConfig.journalFile
    , startDate = cliConfig.startDate
    , isDemo = cliConfig.isDemo
    , pythonExecutable = cliConfig.pythonExecutable
    }
