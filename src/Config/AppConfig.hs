module Config.AppConfig (AppConfig (..), makeAppConfig) where

import Config.CliConfig (CliConfig (..))
import Config.Files (ConfigDirectory)
import Config.YamlConfig (FintsConfig, LedgerConfig (..), YamlConfig (..))
import Control.Applicative ((<|>))
import Data.Time (Day)
import Utils ((??))

data AppConfig = Config
  { fintsConfig :: FintsConfig
  , ledgerConfig :: LedgerConfig
  , -- path where fints2ledger stores it's config files
    configDirectory :: ConfigDirectory
  , journalFile :: FilePath
  , -- the start date to pull the FinTS entries from (format YYYY/MM/DD)
    startDate :: Day
  , pythonExecutable :: String
  , unattended :: Bool
  }
  deriving (Show)

makeAppConfig :: CliConfig -> YamlConfig -> AppConfig
makeAppConfig cliConfig yamlConfig =
  Config
    { fintsConfig = yamlConfig.fints
    , ledgerConfig = yamlConfig.ledger
    , configDirectory = cliConfig.configDirectory
    , journalFile = cliConfig.journalFile <|> yamlConfig.ledger.journalFile ?? "journal.ledger"
    , startDate = cliConfig.startDate
    , pythonExecutable = cliConfig.pythonExecutable
    , unattended = cliConfig.unattended
    }
