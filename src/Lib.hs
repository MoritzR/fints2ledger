module Lib (
  someFunc,
)
where

import App (App, Env (..), PromptResult (..))
import Completion (runCompletion)
import Config.AppConfig (AppConfig (..), getConfig)
import Config.CliConfig (getCliConfig)
import Config.StartupChecks (runStartupChecks)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Map (Map, (!?))
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TLIO
import Options.Applicative (execParser)
import Prompt (transactionsToLedger)
import System.Console.Haskeline (getInputLine)
import System.Directory (doesFileExist)
import Transactions (getExampleTransactions, getTransactionsFromFinTS)
import Utils (createFile)
import Control.Concurrent (threadDelay)

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

  let env =
        Env
          { config = appConfig
          , putStrLn = putStrLn
          , promptForEntry = promptForEntry
          , readFile = TLIO.readFile
          , appendFile = TLIO.appendFile
          , sleep = threadDelay 500_000
          }

  runReaderT (transactionsToLedger transactions) env

ensureFileExists :: FilePath -> IO ()
ensureFileExists path = do
  fileExists <- doesFileExist path
  unless fileExists do
    putStrLn $ "Creating file '" ++ path ++ "' because it did not exist."
    createFile path

promptForEntry :: Map Text Text -> Text -> App (PromptResult Text)
promptForEntry templateMap key = do
  env <- ask
  liftIO $ runCompletion env.config key do
    liftIO $ env.putStrLn ""
    line <- getInputLine $ TL.unpack ("> " <> key <> ": ")
    case line of
      Nothing -> return Skip
      Just "s" -> return Skip
      -- TODO throw error instead? This can happen when the user doesn't provide an account and also sets no default
      Just "" -> return $ maybe Skip Result (templateMap !? key)
      Just value -> return $ Result $ TL.strip $ TL.pack value
