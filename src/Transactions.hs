{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Transactions (
  getTransactionsFromFinTS,
  getExampleTransactions,
  Transaction (..),
  Amount (..),
)
where

import Config.AppConfig (AppConfig (..))
import Config.YamlConfig (FintsConfig (..), Password)
import Control.Exception (Exception, throwIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Text.Lazy.IO qualified as TLIO
import Data.Time (Day, defaultTimeLocale, formatTime)
import GHC.Generics (Generic)
import Hledger (getCurrentDay)
import Paths_fints2ledger (getDataFileName)
import System.Process.Typed (ExitCode (ExitFailure, ExitSuccess), readProcess, shell)
import Utils (encodeAsString, orElseThrow, (??))

getExampleTransactions :: IO [Transaction]
getExampleTransactions = do
  contents <- TLIO.readFile =<< getDataFileName "data/example.json"
  decodeTransactions contents `orElseThrow` TransactionDecodeError
 where
  decodeTransactions = Aeson.eitherDecode . TL.encodeUtf8

getTransactionsFromFinTS :: AppConfig -> IO [Transaction]
getTransactionsFromFinTS config = do
  currentDay <- getCurrentDay
  pyfintsFilePath <- getDataFileName "data/pyfints.py"
  let pyfintsArgs =
        PyFintsArguments
          { account = TL.unpack config.fintsConfig.account
          , blz = TL.unpack config.fintsConfig.blz
          , endpoint = TL.unpack config.fintsConfig.endpoint
          , selectedAccount = TL.unpack $ config.fintsConfig.selectedAccount ?? config.fintsConfig.account
          , password = config.fintsConfig.password
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
      TLIO.putStrLn $ TL.decodeUtf8 stdErr
      throwIO $ PyFintsError "Failed to get FinTS transactions, check the message above."

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
  deriving newtype (Num, Show, Eq)

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
  deriving (Show, Generic, FromJSON, ToJSON, Eq)

newtype TransactionDecodeError = TransactionDecodeError String deriving (Show)

instance Exception TransactionDecodeError

newtype PyFintsError = PyFintsError String deriving (Show)

instance Exception PyFintsError
