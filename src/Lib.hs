module Lib (
  someFunc,
)
where

import Config.AppConfig (AppConfig (..), getConfig)
import Config.CliConfig (getCliConfig)
import Config.StartupChecks (runStartupChecks)
import Control.Monad (unless)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Options.Applicative (execParser)
import Prompt (transactionsToLedger)
import System.Directory (doesFileExist)
import Transactions (getExampleTransactions, getTransactionsFromFinTS)
import Utils (createFile)

data Env = Env
  { config :: AppConfig
  }

someFunc :: IO ()
someFunc = do
  cliConfig <- execParser =<< getCliConfig

  runStartupChecks cliConfig

  appConfig <- getConfig cliConfig

  ensureFileExists appConfig.journalFile

  let getTransactions =
        if appConfig.isDemo
          then getExampleTransactions
          else getTransactionsFromFinTS appConfig
  transactions <- getTransactions

  let env = Env{config = appConfig}

  runReaderT (transactionsToLedger transactions) env

ensureFileExists :: FilePath -> IO ()
ensureFileExists path = do
  fileExists <- doesFileExist path
  unless fileExists do
    putStrLn $ "Creating file '" ++ path ++ "' because it did not exist."
    createFile path
