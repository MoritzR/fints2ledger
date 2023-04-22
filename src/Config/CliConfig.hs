module Config.CliConfig (getCliConfig, CliConfig (..)) where

import Config.Files (ConfigDirectory, getDefaultConfigDirectory)
import Data.Dates (DateTime, dateTimeToDay, getCurrentDateTime, parseDate)
import Data.Time (Day, addDays)
import Hledger (getCurrentDay)
import Options.Applicative (Parser, ParserInfo, eitherReader, fullDesc, header, help, info, long, metavar, option, progDesc, short, showDefault, strOption, switch, value)

data CliConfig = CliConfig
  { configDirectory :: ConfigDirectory
  , journalFile :: FilePath
  , startDate :: Day
  , isDemo :: Bool
  }
  deriving (Show)

configDirectoryOption :: ConfigDirectory -> Parser ConfigDirectory
configDirectoryOption defaultDirectory =
  strOption $
    long "files-path"
      <> help "directory where fints2ledger puts it's config files (like config.yml)"
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
        <> help "Start date to pull the FinTS transactions from" -- TODO better help text
        <> metavar "DATE"
        <> showDefault
        <> value defaultValue
    )

demoOption :: Parser Bool
demoOption =
  switch $
    long "demo"
      <> help "runs with a sample set of transactions, without actually calling a FinTS endpoint"

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
      <*> demoOption

getCliConfig :: IO (ParserInfo CliConfig)
getCliConfig = do
  parser <- getCliParser
  return $
    info
      parser
      ( fullDesc
          -- TODO fill in correct help text
          <> progDesc "Print a greeting for TARGET"
          <> header "hello - a test for optparse-applicative"
      )

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x
