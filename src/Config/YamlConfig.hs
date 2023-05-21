{-# LANGUAGE DeriveAnyClass #-}

module Config.YamlConfig (
  YamlConfig (..),
  FintsConfig (..),
  LedgerConfig (..),
  Filling (..),
  Fill,
  Password,
  getYamlConfig,
  defaultYamlConfig,
  validateYamlConfig,
  writeYamlConfig,
)
where

import Config.Files (ConfigDirectory, getConfigFilePath)
import Control.Exception (Exception, throwIO)
import Data.Aeson as Aeson
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set, difference, isSubsetOf, toList)
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)

data YamlConfig = YamlConfig
  { fints :: FintsConfig
  , ledger :: LedgerConfig
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data FintsConfig = FintsConfig
  { account :: Text
  , blz :: Text
  , endpoint :: Text
  , selectedAccount :: Maybe Text
  , password :: Password
  }
  deriving (Show, Eq, Generic)

instance ToJSON FintsConfig where
  toJSON =
    genericToJSON
      defaultOptions
        { omitNothingFields = True
        }

instance FromJSON FintsConfig where
  parseJSON =
    genericParseJSON
      defaultOptions
        { omitNothingFields = True
        }

data LedgerConfig = LedgerConfig
  { defaults :: DefaultsConfig
  , md5 :: [String]
  , prompts :: [Text]
  , fills :: [Filling]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Filling = Filling
  { match :: Match
  , fill :: Fill
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- a Map from a field to a default value for this field
type DefaultsConfig = Map Text Text

-- a Map from the field to a pattern to match on
type Match = Map Text Text

-- a Map from the field to fill to the content that should be filled in
-- an empty filling means that this field should still be prompted for
type Fill = Map Text (Maybe Text)

newtype Password = Password {get :: Text}
  deriving (Generic, Eq)

instance FromJSON Password where
  parseJSON = fmap Password . parseJSON

instance ToJSON Password where
  toJSON value = toJSON value.get

instance Show Password where
  show = const "********"

getYamlConfig :: ConfigDirectory -> IO YamlConfig
getYamlConfig configDirectory = do
  config <- Yaml.decodeFileThrow $ getConfigFilePath configDirectory
  case validateYamlConfig config of
    Nothing -> return config
    Just err -> throwIO $ ValidationException err

allowedKeySet :: Set String
allowedKeySet = Set.fromList ["date", "amount", "currency", "posting", "payee", "purpose"]

validateYamlConfig :: YamlConfig -> Maybe String
validateYamlConfig yamlConfig = do
  if md5KeySet `isSubsetOf` allowedKeySet
    then Nothing
    else
      Just $
        "md5 values are not valid: "
          <> (show . toList) (md5KeySet `difference` allowedKeySet)
          <> ", only "
          <> (show . toList) allowedKeySet
          <> " are allowed"
 where
  md5KeySet = Set.fromList yamlConfig.ledger.md5
defaultYamlConfig :: YamlConfig
defaultYamlConfig =
  YamlConfig
    { fints =
        FintsConfig
          { blz = "<your banks BLZ>"
          , account = "<your account number>"
          , password = Password "<your banking password>"
          , endpoint = "<your bank fints endpoint>"
          , selectedAccount = Nothing
          }
    , ledger =
        LedgerConfig
          { prompts = ["credit_account"]
          , defaults = Map.fromList [("debit_account", "assets:bank:checking")]
          , md5 = ["date", "payee", "purpose", "amount"]
          , fills = []
          }
    }

writeYamlConfig :: FilePath -> YamlConfig -> IO ()
writeYamlConfig = Yaml.encodeFile

newtype ValidationException = ValidationException String deriving (Show)

instance Exception ValidationException
