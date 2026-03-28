module Config.AppConfig (AppConfig (..), makeAppConfig) where

import Config.CliConfig (CliConfig (..))
import Config.EnvConfig qualified as Env (EnvConfig (..))
import Config.Files (ConfigDirectory)
import Config.YamlConfig (FintsConfig (..), LedgerConfig (..), YamlConfig (..))
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
  }
  deriving (Show)

makeAppConfig :: CliConfig -> YamlConfig -> Env.EnvConfig -> AppConfig
makeAppConfig cliConfig yamlConfig envConfig =
  Config
    { fintsConfig = yamlConfig.fints{password = envConfig.password <|> yamlConfig.fints.password}
    , ledgerConfig = yamlConfig.ledger
    , configDirectory = cliConfig.configDirectory
    , journalFile = cliConfig.journalFile <|> yamlConfig.ledger.journalFile ?? "journal.ledger"
    , startDate = cliConfig.startDate
    , pythonExecutable = cliConfig.pythonExecutable
    }
