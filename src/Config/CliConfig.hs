{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Config.CliConfig (getCliConfig, CliConfig (..)) where

import Config.Files (ConfigDirectory, getDefaultConfigDirectory)
import Data.Time (Day, addDays, defaultTimeLocale, formatTime)
import Data.Version (showVersion)
import Dates.Parser (parseDate)
import Hledger (getCurrentDay)
import Options.Applicative (Parser, ParserInfo, eitherReader, fullDesc, help, helper, info, long, metavar, option, optional, progDesc, short, showDefault, showDefaultWith, simpleVersioner, strOption, switch, value, (<**>))
import Paths_fints2ledger (version)

data CliConfig = CliConfig
  { configDirectory :: ConfigDirectory
  , journalFile :: Maybe FilePath
  , startDate :: Day
  , pythonExecutable :: String
  , isDemo :: Bool
  , shouldEditConfig :: Bool
  , unattended :: Bool
  , fromCsvFile :: Maybe FilePath
  , toCsvFile :: Maybe FilePath
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

journalFileOption :: Parser (Maybe FilePath)
journalFileOption =
  optional $
    strOption $
      long "journal-file"
        <> short 'f'
        <> help "path to the journal file"
        <> metavar "FILE"

startDateOption :: Day -> Day -> Parser Day
startDateOption currentDay defaultValue =
  option
    (eitherReader $ parseDate currentDay)
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

unattendedOption :: Parser Bool
unattendedOption =
  switch $
    long "unattended"
      <> help "Runs without any prompting for input. Will apply automatic transaction matching and report number of transactions that need to be manually matched."

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

toCsvFileOption :: Parser (Maybe FilePath)
toCsvFileOption =
  optional $
    strOption $
      long "to-csv-file"
        <> metavar "FILE"
        <> help "Write transactions to this csv file instead of to a ledger journal"

fromCsvFileOption :: Parser (Maybe FilePath)
fromCsvFileOption =
  optional $
    strOption $
      long "from-csv-file"
        <> metavar "FILE"
        <> help "Read transactions from this csv file instead of from a FinTS endpoint"

getCliParser :: IO (Parser CliConfig)
getCliParser = do
  defaultConfigDirectory <- getDefaultConfigDirectory
  currentDay <- getCurrentDay
  let ninetyDaysAgo = addDays (-90) currentDay
  return do
    configDirectory <- configDirectoryOption defaultConfigDirectory
    journalFile <- journalFileOption
    startDate <- startDateOption currentDay ninetyDaysAgo
    pythonExecutable <- pythonExecutableOption
    isDemo <- demoOption
    shouldEditConfig <- configOption
    toCsvFile <- toCsvFileOption
    fromCsvFile <- fromCsvFileOption
    unattended <- unattendedOption
    pure CliConfig{..}

getCliConfig :: IO (ParserInfo CliConfig)
getCliConfig = do
  parser <- getCliParser
  return $
    info
      (parser <**> helper <**> simpleVersioner (showVersion version))
      ( fullDesc
          <> progDesc "Convert banking transactions from a FinTS endpoint to a ledger journal."
      )
