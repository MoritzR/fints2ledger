module Completion (
  runCompletion,
)
where

import Config.AppConfig (AppConfig (..))
import Data.Function ((&))
import Data.List (stripPrefix)
import Data.Text (Text, isPrefixOf, pack, unpack)
import Hledger (Journal)
import Ledger (getAccounts, getJournal)
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
  journal <- getJournal config.journalFile
  runInputT (makeSettings $ accountCompletion journal) input
 where
  accountCompletion journal = completeWord Nothing [] (return . getCompletions journal)

runWithNoCompletion :: InputT IO a -> IO a
runWithNoCompletion = runInputT $ makeSettings noCompletion

getCompletions :: Journal -> String -> [Completion]
getCompletions journal input =
  getAccounts journal
    & filter (isPrefixOf $ pack input)
    & map (completion . unpack)
 where
  completion string = Completion string (stripPrefix input string ?? string) False

makeSettings :: CompletionFunc m -> Settings m
makeSettings completion = Settings completion Nothing False
