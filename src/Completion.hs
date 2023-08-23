module Completion (
  runCompletion,
)
where

import Config.AppConfig (AppConfig (..))
import Data.Function ((&))
import Data.Text (Text, isPrefixOf, pack, stripPrefix, unpack)
import Hledger (AccountName)
import Ledger (getAccounts)
import System.Console.Haskeline (Completion (Completion), CompletionFunc, InputT, Settings (Settings), completeWord, noCompletion, runInputT)
import Utils ((??))

-- | Provides tab completion when prompting for input
runCompletion :: AppConfig -> Text -> (forall a. InputT IO a -> IO a)
runCompletion config key
  -- make this elem check more robust to renames
  | key `elem` ["credit_account", "debit_account"] = runWithAccountCompletion config
  | otherwise = runWithNoCompletion

runWithAccountCompletion :: AppConfig -> InputT IO a -> IO a
runWithAccountCompletion config input = do
  accounts <- getAccounts config.journalFile
  let accountCompletion = completeWord Nothing [] (return . getCompletions accounts . pack)
  runInputT (makeSettings accountCompletion) input

runWithNoCompletion :: InputT IO a -> IO a
runWithNoCompletion = runInputT $ makeSettings noCompletion

getCompletions :: [AccountName] -> Text -> [Completion]
getCompletions accounts input =
  accounts
    & filter isPrefixOfInput
    & map completion
 where
  isPrefixOfInput = isPrefixOf input
  completion account = Completion (unpack account) (unpack $ stripPrefix input account ?? account) False

makeSettings :: CompletionFunc m -> Settings m
makeSettings completion = Settings completion Nothing False
