module Ledger (getAccounts, getJournal) where

import Control.Exception (Exception, throwIO)
import Data.List (nub)
import Hledger (AccountName, Journal, Posting (paccount), definputopts, journalPostings, readJournalFile, runExceptT)

newtype JournalReadException = JournalReadException String deriving (Show)

instance Exception JournalReadException

getAccounts :: Journal -> [AccountName]
getAccounts = nub . map paccount . journalPostings

getJournal :: FilePath -> IO Journal
getJournal path = do
  maybeJournal <- runExceptT $ readJournalFile definputopts path
  case maybeJournal of
    Left err -> throwIO $ JournalReadException err
    Right journal -> return journal
