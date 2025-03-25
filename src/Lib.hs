module Lib (
  runFints2Ledger,
)
where

import App (App, Env (..), PromptResult (..))
import Completion (runCompletion)
import Config.AppConfig (AppConfig (..), makeAppConfig)
import Config.CliConfig (CliConfig (..), getCliConfig)
import Config.Files (getConfigFilePath)
import Config.StartupChecks (runStartupChecks)
import Config.YamlConfig (defaultYamlConfig, getYamlConfig, writeYamlConfig)
import Control.Concurrent (threadDelay)
import Control.Exception (Exception)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Foldable (for_)
import Data.Map (Map, (!?))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative (execParser)
import Prompt (transactionsToLedger)
import System.Console.Haskeline (getInputLine)
import System.Directory (doesFileExist)
import Transactions (Transaction, getExampleTransactions, getTransactionsFromCsv, getTransactionsFromFinTS, writeTransactionsToCsv)
import UI.ConfigUI (runConfigUI)
import Utils (createFile)

runFints2Ledger :: IO ()
runFints2Ledger = do
  cliConfig <- execParser =<< getCliConfig

  appConfig <-
    if cliConfig.isDemo
      then return $ makeAppConfig cliConfig defaultYamlConfig
      else do
        runStartupChecks cliConfig
        yamlConfig <- getYamlConfig cliConfig.configDirectory
        return $ makeAppConfig cliConfig yamlConfig

  ensureFileExists appConfig.journalFile

  if cliConfig.shouldEditConfig
    then editConfig appConfig
    else run cliConfig appConfig

run :: CliConfig -> AppConfig -> IO ()
run cliConfig appConfig = getTransactions >>= convertTransactions
 where
  getTransactions =
    if cliConfig.isDemo
      then getExampleTransactions
      else maybe (getTransactionsFromFinTS appConfig) getTransactionsFromCsv cliConfig.fromCsvFile
  convertTransactions = maybe (convertTransactionsForConfig appConfig) writeTransactionsToCsv cliConfig.toCsvFile

editConfig :: AppConfig -> IO ()
editConfig appConfig = do
  yamlConfig <- getYamlConfig appConfig.configDirectory
  maybeUiConfig <- runConfigUI yamlConfig appConfig.configDirectory
  for_ maybeUiConfig do
    writeYamlConfig (getConfigFilePath appConfig.configDirectory)

convertTransactionsForConfig :: AppConfig -> [Transaction] -> IO ()
convertTransactionsForConfig appConfig transactions = do
  let env =
        Env
          { config = appConfig
          , putStrLn = TIO.putStrLn
          , promptForEntry = promptForEntry
          , readFile = TIO.readFile
          , appendFile = TIO.appendFile
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
    line <- getInputLine $ T.unpack ("> " <> key <> ": ")
    case line of
      Nothing -> return Skip
      Just "s" -> return Skip
      -- TODO throw error instead? This can happen when the user doesn't provide an account and also sets no default
      Just "" -> return $ maybe Skip Result (templateMap !? key)
      Just value -> return $ Result $ T.strip $ T.pack value

newtype CliOptionsError = CliOptionsError String deriving (Show)
instance Exception CliOptionsError
