{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Config.Files (getDefaultConfigDirectory, getConfigFilePath, templateFile, exampleFile, pyfintsFile, ConfigDirectory (..)) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir)
import Data.Map (Map, fromList, (!))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

getDefaultConfigDirectory :: IO ConfigDirectory
getDefaultConfigDirectory = do
  homeDirectory <- getHomeDirectory
  return $ ConfigDirectory $ homeDirectory </> ".config" </> "fints2ledger"

getConfigFilePath :: ConfigDirectory -> FilePath
getConfigFilePath configDirectory = configDirectory.get </> "config.yml"

templateFile :: Text
templateFile = T.decodeUtf8 $ dataFiles ! "template.txt"

exampleFile :: Text
exampleFile = T.decodeUtf8 $ dataFiles ! "example.json"

pyfintsFile :: Text
pyfintsFile = T.decodeUtf8 $ dataFiles ! "pyfints.py"

dataFiles :: Map FilePath ByteString
dataFiles = fromList ($(embedDir "data"))

newtype ConfigDirectory = ConfigDirectory {get :: FilePath}
  deriving newtype (Show, IsString)
