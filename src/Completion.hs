module Completion (
  runCompletion,
)
where

import Config.AppConfig (AppConfig (..))
import Data.Function ((&))
import Data.List (stripPrefix)
import Data.Text (isPrefixOf, pack, unpack)
import Data.Text.Lazy (Text)
import Hledger (Journal)
import Ledger (getAccounts, getJournal)
import System.Console.Haskeline (Completion (Completion), CompletionFunc, InputT, Settings (Settings), completeWord, noCompletion, runInputT)
import Utils ((??))

runCompletion :: AppConfig -> Text -> (forall a. InputT IO a -> IO a)
runCompletion config key
  -- make this elem check more robust to renames
  | key `elem` ["credit_account", "debit_account"] = runWithAccountCompletion config
  | otherwise = runWithNoCompletion

getCompletions :: Journal -> String -> [Completion]
getCompletions journal input =
  getAccounts journal
    & filter (isPrefixOf $ pack input)
    & map (completion . unpack)
 where
  completion string = Completion string (stripPrefix input string ?? string) False

accountCompletion :: Journal -> CompletionFunc IO
accountCompletion journal = completeWord Nothing [] (return . getCompletions journal)

makeSettings :: CompletionFunc m -> Settings m
makeSettings completion = Settings completion Nothing False

runWithAccountCompletion :: AppConfig -> InputT IO a -> IO a
runWithAccountCompletion config input = do
  journal <- getJournal config.journalFile
  runInputT (makeSettings $ accountCompletion journal) input

runWithNoCompletion :: InputT IO a -> IO a
runWithNoCompletion = runInputT $ makeSettings noCompletion
