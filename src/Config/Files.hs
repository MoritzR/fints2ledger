{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Config.Files (getDefaultConfigDirectory, getConfigFilePath, getTemplateFile, exampleFile, pyfintsFile, ConfigDirectory (..)) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir)
import Data.Map (Map, fromList, (!))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath ((</>))

getDefaultConfigDirectory :: IO ConfigDirectory
getDefaultConfigDirectory = do
  homeDirectory <- getHomeDirectory
  return $ ConfigDirectory $ homeDirectory </> ".config" </> "fints2ledger"

getConfigFilePath :: ConfigDirectory -> FilePath
getConfigFilePath configDirectory = configDirectory.get </> "config.yml"

getTemplateFile :: ConfigDirectory -> (FilePath -> IO Text) -> IO Text
getTemplateFile configDirectory readTextFile = do
  let templateFilePath = configDirectory.get </> "template.txt"
  fileExists <- doesFileExist templateFilePath
  if fileExists
    then readTextFile templateFilePath
    else return $ T.decodeUtf8 $ dataFiles ! "template.txt"

exampleFile :: Text
exampleFile = T.decodeUtf8 $ dataFiles ! "example.json"

pyfintsFile :: Text
pyfintsFile = T.decodeUtf8 $ dataFiles ! "pyfints.py"

dataFiles :: Map FilePath ByteString
dataFiles = fromList ($(embedDir "data"))

newtype ConfigDirectory = ConfigDirectory {get :: FilePath}
  deriving newtype (Show, IsString)
