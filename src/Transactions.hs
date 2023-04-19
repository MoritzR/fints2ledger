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
import Data.Text.Lazy.Encoding qualified as TL
import Data.Text.Lazy.IO qualified as TLIO
import GHC.Generics (Generic)
import Paths_hsfints2ledger (getDataFileName)
import System.Process.Typed (ExitCode (ExitFailure, ExitSuccess), readProcess, shell)
import Utils (encodeAsString, orElseThrow, (??))
import Data.Text.Lazy (Text)

getExampleTransactions :: IO [Transaction]
getExampleTransactions = do
  contents <- TLIO.readFile =<< getDataFileName "data/example.json"
  decodeTransactions contents `orElseThrow` TransactionDecodeError
 where
  decodeTransactions = Aeson.eitherDecode . TL.encodeUtf8

getTransactionsFromFinTS :: AppConfig -> IO [Transaction]
getTransactionsFromFinTS config = do
  pyfintsFilePath <- getDataFileName "data/pyfints.py"
  let pyfintsArgs =
        PyFintsArguments
          { account = config.fintsConfig.account
          , blz = config.fintsConfig.blz
          , endpoint = config.fintsConfig.endpoint
          , selectedAccount = config.fintsConfig.selectedAccount ?? config.fintsConfig.account
          , password = config.fintsConfig.password
          , start = "2023/03/01"
          , end = "2023/04/01"
          }
  let shellCommand =
        shell $
          "FINTS2LEDGER_ARGS='"
            ++ encodeAsString pyfintsArgs
            ++ "' python3 "
            ++ pyfintsFilePath
  (exitCode, stdOut, stdErr) <- readProcess shellCommand
  case exitCode of
    ExitSuccess -> Aeson.eitherDecode stdOut `orElseThrow` TransactionDecodeError
    ExitFailure _ -> do
      TLIO.putStrLn $ TL.decodeUtf8 stdErr
      throwIO $ PyFintsError "Failed to get FinTS transactions, check the message above."

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
