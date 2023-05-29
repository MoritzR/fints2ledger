module Config.CliConfig (getCliConfig, CliConfig (..)) where

import Config.Files (ConfigDirectory, getDefaultConfigDirectory)
import Data.Dates (DateTime, dateTimeToDay, getCurrentDateTime, parseDate)
import Data.Time (Day, addDays, defaultTimeLocale, formatTime)
import Hledger (getCurrentDay)
import Options.Applicative (Parser, ParserInfo, eitherReader, fullDesc, help, helper, info, long, metavar, option, progDesc, short, showDefault, showDefaultWith, strOption, switch, value, (<**>))

data CliConfig = CliConfig
  { configDirectory :: ConfigDirectory
  , journalFile :: FilePath
  , startDate :: Day
  , pythonExecutable :: String
  , isDemo :: Bool
  , shouldEditConfig :: Bool
  }
  deriving (Show)

configDirectoryOption :: ConfigDirectory -> Parser ConfigDirectory
configDirectoryOption defaultDirectory =
  strOption $
    long "files-path"
      <> help "directory where fints2ledger puts its config files (like config.yml)"
      <> metavar "PATH"
      <> showDefault
      <> value defaultDirectory

journalFileOption :: Parser FilePath
journalFileOption =
  strOption $
    long "journal-file"
      <> short 'f'
      <> help "path to the journal file"
      <> metavar "FILE"
      <> showDefault
      <> value "journal.ledger"

startDateOption :: DateTime -> Day -> Parser Day
startDateOption currentDateTime defaultValue =
  option
    (eitherReader $ fmap dateTimeToDay . mapLeft show . parseDate currentDateTime)
    ( long "date"
        <> help "Start date to pull the FinTS transactions from. Possible values are for example: '25.01.2023', '90 days ago', 'last monday'"
        <> metavar "DATE"
        <> showDefaultWith (formatTime defaultTimeLocale "%d.%m.%Y")
        <> value defaultValue
    )

demoOption :: Parser Bool
demoOption =
  switch $
    long "demo"
      <> help "runs with a sample set of transactions, without actually calling a FinTS endpoint"

configOption :: Parser Bool
configOption =
  switch $
    long "config"
      <> help "show a UI for editing the config"

pythonExecutableOption :: Parser FilePath
pythonExecutableOption =
  strOption $
    long "python-command"
      <> help "command for running python"
      <> showDefault
      <> value "python3"

getCliParser :: IO (Parser CliConfig)
getCliParser = do
  defaultConfigDirectory <- getDefaultConfigDirectory
  ninetyDaysAgo <- addDays (-90) <$> getCurrentDay
  currentDateTime <- getCurrentDateTime
  return $
    CliConfig
      <$> configDirectoryOption defaultConfigDirectory
      <*> journalFileOption
      <*> startDateOption currentDateTime ninetyDaysAgo
      <*> pythonExecutableOption
      <*> demoOption
      <*> configOption

getCliConfig :: IO (ParserInfo CliConfig)
getCliConfig = do
  parser <- getCliParser
  return $
    info
      (parser <**> helper)
      ( fullDesc
          <> progDesc "Convert banking transactions from a FinTS endpoint to a ledger journal."
      )

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x
