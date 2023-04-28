{-# LANGUAGE DerivingStrategies #-}

module Config.Files (getDefaultConfigDirectory, getConfigFilePath, getTemplatePath, ConfigDirectory (..)) where

import Data.String (IsString)
import Paths_fints2ledger (getDataFileName)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

getDefaultConfigDirectory :: IO ConfigDirectory
getDefaultConfigDirectory = do
  homeDirectory <- getHomeDirectory
  return $ ConfigDirectory $ homeDirectory </> ".config" </> "fints2ledger"

getConfigFilePath :: ConfigDirectory -> FilePath
getConfigFilePath configDirectory = configDirectory.get </> "config.yml"

getTemplatePath :: IO FilePath
getTemplatePath = getDataFileName "data/template.txt"

newtype ConfigDirectory = ConfigDirectory {get :: FilePath}
  deriving newtype (Show, IsString)
