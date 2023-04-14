module Completion (
  runWithAccountCompletion,
  runWithNoCompletion,
)
where

import Config.AppConfig (AppConfig (..))
import Data.Function ((&))
import Data.Text (isPrefixOf, pack, unpack)
import Hledger (Journal)
import Ledger (getAccounts, getJournal)
import System.Console.Haskeline (Completion, CompletionFunc, InputT, Settings (Settings), completeWord, noCompletion, runInputT, simpleCompletion)

getCompletions :: Journal -> String -> [Completion]
getCompletions journal input =
  getAccounts journal
    & filter (isPrefixOf $ pack input)
    & map (simpleCompletion . unpack)

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
