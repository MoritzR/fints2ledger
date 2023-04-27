module Main (main) where

import System.Environment (getExecutablePath, setEnv)
import System.FilePath (takeDirectory)
import Lib

main :: IO ()
main = do
    setEnv "hsfints2ledger_datadir" =<< (takeDirectory <$> getExecutablePath)
    runFints2Ledger
