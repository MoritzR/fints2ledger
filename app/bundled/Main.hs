module Main (main) where

import Lib
import System.Environment (getExecutablePath, setEnv)
import System.FilePath (takeDirectory)

main :: IO ()
main = do
  setEnv "fints2ledger_datadir" =<< (takeDirectory <$> getExecutablePath)
  runFints2Ledger
