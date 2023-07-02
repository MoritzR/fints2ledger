{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Config.CliConfig (getCliConfig, CliConfig (..), CsvMode (..), csvModeFromConfig) where

import Config.Files (ConfigDirectory, getDefaultConfigDirectory)
import Data.Dates (DateTime, dateTimeToDay, getCurrentDateTime, parseDate)
import Data.Time (Day, addDays, defaultTimeLocale, formatTime)
import Hledger (getCurrentDay)
import Options.Applicative (Parser, ParserInfo, eitherReader, fullDesc, help, helper, info, long, metavar, option, optional, progDesc, short, showDefault, showDefaultWith, strOption, switch, value, (<**>))

data CsvMode = ToFile FilePath | FromFile FilePath | NoCsv | FromTo deriving (Show)

data CliConfig = CliConfig
  { configDirectory :: ConfigDirectory
  , journalFile :: FilePath
  , startDate :: Day
  , pythonExecutable :: String
  , isDemo :: Bool
  , shouldEditConfig :: Bool
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

toCsvFileOption :: Parser (Maybe FilePath)
toCsvFileOption =
  optional $
    strOption $
      long "to-csv-file"
        <> help "Write transactions to this csv file instead of to a ledger journal"

fromCsvFileOption :: Parser (Maybe FilePath)
fromCsvFileOption =
  optional $
    strOption $
      long "from-csv-file"
        <> help "Read transactions from this csv file instead of from a FinTS endpoint"

getCliParser :: IO (Parser CliConfig)
getCliParser = do
  defaultConfigDirectory <- getDefaultConfigDirectory
  ninetyDaysAgo <- addDays (-90) <$> getCurrentDay
  currentDateTime <- getCurrentDateTime
  return $ do
    configDirectory <- configDirectoryOption defaultConfigDirectory
    journalFile <- journalFileOption
    startDate <- startDateOption currentDateTime ninetyDaysAgo
    pythonExecutable <- pythonExecutableOption
    isDemo <- demoOption
    shouldEditConfig <- configOption
    toCsvFile <- toCsvFileOption
    fromCsvFile <- fromCsvFileOption
    pure CliConfig{..}

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

csvModeFromConfig :: CliConfig -> CsvMode
csvModeFromConfig config =
  case config.toCsvFile of
    Just toCsvPath -> maybe (ToFile toCsvPath) (const FromTo) (config.fromCsvFile)
    Nothing -> maybe NoCsv FromFile (config.fromCsvFile)
