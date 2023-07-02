module Lib (
  runFints2Ledger,
)
where

import App (App, Env (..), PromptResult (..))
import Completion (runCompletion)
import Config.AppConfig (AppConfig (..), makeAppConfig)
import Config.CliConfig (CliConfig (..), CsvMode (..), csvModeFromConfig, getCliConfig)
import Config.Files (getConfigFilePath)
import Config.StartupChecks (runStartupChecks)
import Config.YamlConfig (YamlConfig (..), defaultYamlConfig, getYamlConfig, writeYamlConfig)
import Control.Concurrent (threadDelay)
import Control.Exception (Exception, throw)
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
import Transactions (Transaction, convertTransactionsToCsv, getExampleTransactions, getTransactionsFromCsv, getTransactionsFromFinTS)
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

  run cliConfig appConfig

run :: CliConfig -> AppConfig -> IO ()
run cliConfig appConfig
  | cliConfig.isDemo =
      getExampleTransactions
        >>= convertTransactionsForConfig appConfig
  | cliConfig.shouldEditConfig = do
      yamlConfig <- getYamlConfig appConfig.configDirectory
      maybeConfig <- runConfigUI yamlConfig appConfig.configDirectory
      for_ maybeConfig \fintsConfig ->
        writeYamlConfig (getConfigFilePath appConfig.configDirectory) yamlConfig{fints = fintsConfig}
  | otherwise = case csvModeFromConfig cliConfig of
      NoCsv ->
        getTransactionsFromFinTS appConfig
          >>= convertTransactionsForConfig appConfig
      FromFile path ->
        getTransactionsFromCsv path
          >>= convertTransactionsForConfig appConfig
      ToFile path ->
        getTransactionsFromFinTS appConfig
          >>= convertTransactionsToCsv path
      FromTo -> throw $ CliOptionsError "don't use both from csv file and to csv file flags simultaniously"

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
