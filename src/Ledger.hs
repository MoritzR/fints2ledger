module Ledger (getAccounts) where

import Control.Exception (Exception, throwIO)
import Data.List (nub)
import Hledger (AccountName, Journal, Posting (paccount), definputopts, journalPostings, readJournalFile, runExceptT)

newtype JournalReadException = JournalReadException String deriving (Show)

instance Exception JournalReadException

accounts :: Journal -> [AccountName]
accounts = nub . map paccount . journalPostings

getAccounts :: FilePath -> IO [AccountName]
getAccounts path = do
  maybeJournal <- runExceptT $ readJournalFile definputopts path
  case maybeJournal of
    Left err -> throwIO $ JournalReadException err
    Right journal -> return $ accounts journal
