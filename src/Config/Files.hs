{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Config.Files (getDefaultConfigDirectory, defaultTemplateFile, getConfigFilePath, getTemplateFile, exampleFile, pyfintsFile, ConfigDirectory (..)) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir)
import Data.Map (Map, fromList, (!))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import System.Directory (XdgDirectory (XdgConfig), doesFileExist, getXdgDirectory)
import System.FilePath ((</>))

getDefaultConfigDirectory :: IO ConfigDirectory
getDefaultConfigDirectory = ConfigDirectory <$> getXdgDirectory XdgConfig "fints2ledger"

getConfigFilePath :: ConfigDirectory -> FilePath
getConfigFilePath configDirectory = configDirectory.get </> "config.yml"

getTemplateFile :: ConfigDirectory -> (FilePath -> IO Text) -> IO Text
getTemplateFile configDirectory readTextFile = do
  let templateFilePath = configDirectory.get </> "template.txt"
  fileExists <- doesFileExist templateFilePath
  if fileExists
    then readTextFile templateFilePath
    else return defaultTemplateFile

defaultTemplateFile :: Text
defaultTemplateFile = T.decodeUtf8 $ dataFiles ! "template.txt"

exampleFile :: Text
exampleFile = T.decodeUtf8 $ dataFiles ! "example.json"

pyfintsFile :: Text
pyfintsFile = T.decodeUtf8 $ dataFiles ! "pyfints.py"

dataFiles :: Map FilePath ByteString
dataFiles = fromList ($(embedDir "data"))

newtype ConfigDirectory = ConfigDirectory {get :: FilePath}
  deriving newtype (Show, IsString)
