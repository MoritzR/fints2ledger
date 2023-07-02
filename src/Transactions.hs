{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Transactions (
  getTransactionsFromFinTS,
  getExampleTransactions,
  getTransactionsFromCsv,
  convertTransactionsToCsv,
  Transaction (..),
  Amount (..),
)
where

import Config.AppConfig (AppConfig (..))
import Config.YamlConfig (FintsConfig (..), Password (..))
import Control.Exception (Exception, throwIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BS
import Data.Csv (DefaultOrdered, FromField, FromNamedRecord, ToField, ToNamedRecord)
import Data.Csv qualified as Csv
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time (Day, defaultTimeLocale, formatTime)
import Data.Vector (toList)
import GHC.Generics (Generic)
import Hledger (getCurrentDay)
import Paths_fints2ledger (getDataFileName)
import System.Console.Haskeline qualified as Haskeline
import System.Process.Typed (ExitCode (ExitFailure, ExitSuccess), readProcess, shell)
import Utils (encodeAsString, orElseThrow, (??))

getExampleTransactions :: IO [Transaction]
getExampleTransactions = do
  contents <- TIO.readFile =<< getDataFileName "data/example.json"
  decodeTransactions contents `orElseThrow` TransactionDecodeError
 where
  decodeTransactions = Aeson.eitherDecode . TL.encodeUtf8 . TL.fromStrict

getTransactionsFromCsv :: FilePath -> IO [Transaction]
getTransactionsFromCsv path = do
  csvContents <- BS.readFile path
  case Csv.decodeByName csvContents of
    Right (_header, rows) -> return $ toList rows
    Left message -> throwIO $ CsvDecodeError message

convertTransactionsToCsv :: FilePath -> [Transaction] -> IO ()
convertTransactionsToCsv path = BS.writeFile path . Csv.encodeDefaultOrderedByName

getTransactionsFromFinTS :: AppConfig -> IO [Transaction]
getTransactionsFromFinTS config = do
  currentDay <- getCurrentDay
  pyfintsFilePath <- getDataFileName "data/pyfints.py"
  password <- maybe getPassword return (config.fintsConfig.password)

  let pyfintsArgs =
        PyFintsArguments
          { account = T.unpack config.fintsConfig.account
          , blz = T.unpack config.fintsConfig.blz
          , endpoint = T.unpack config.fintsConfig.endpoint
          , selectedAccount = T.unpack $ config.fintsConfig.selectedAccount ?? config.fintsConfig.account
          , password = password
          , start = formatDayForPython config.startDate
          , end = formatDayForPython currentDay
          }
  let shellCommand =
        shell $
          "FINTS2LEDGER_ARGS='"
            ++ encodeAsString pyfintsArgs
            ++ "' "
            ++ config.pythonExecutable
            ++ " "
            ++ pyfintsFilePath
  (exitCode, stdOut, stdErr) <- readProcess shellCommand
  case exitCode of
    ExitSuccess -> Aeson.eitherDecode stdOut `orElseThrow` TransactionDecodeError
    ExitFailure _ -> do
      TIO.putStrLn $ TL.toStrict $ TL.decodeUtf8 stdErr
      throwIO $ PyFintsError "Failed to get FinTS transactions, check the message above."

getPassword :: IO Password
getPassword = do
  Haskeline.runInputT Haskeline.defaultSettings do
    maybePassword <- Haskeline.getPassword (Just '*') "Banking Password: "
    Haskeline.outputStrLn ""
    return $ Password $ T.pack (maybePassword ?? "")

formatDayForPython :: Day -> String
formatDayForPython = formatTime defaultTimeLocale "%Y/%m/%d"

data PyFintsArguments = PyFintsArguments
  { account :: String
  , blz :: String
  , endpoint :: String
  , selectedAccount :: String
  , password :: Password
  , start :: String
  , end :: String
  }
  deriving (Generic, ToJSON)

newtype Amount = Amount {amount :: Double}
  deriving newtype (Num, Show, Eq, FromField, ToField)

instance FromJSON Amount where
  parseJSON value = Amount . read <$> Aeson.parseJSON value

instance ToJSON Amount where
  toJSON value = Aeson.toJSON value.amount

data Transaction = Transaction
  { date :: Text
  , amount :: Amount
  , currency :: Text
  , posting :: Text
  , payee :: Text
  , purpose :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, Eq, DefaultOrdered)

instance FromNamedRecord Transaction
instance ToNamedRecord Transaction

newtype TransactionDecodeError = TransactionDecodeError String deriving (Show)

instance Exception TransactionDecodeError

newtype PyFintsError = PyFintsError String deriving (Show)

instance Exception PyFintsError

newtype CsvDecodeError = CsvDecodeError String deriving (Show)

instance Exception CsvDecodeError
