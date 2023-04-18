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
)
where

import Config.Files (ConfigDirectory, getConfigFilePath)
import Data.Aeson as Aeson
import Data.Map (Map, fromList)
import Data.Text.Lazy (Text)
import Data.Yaml (decodeFileThrow)
import GHC.Generics (Generic)

data YamlConfig = YamlConfig
  { fints :: FintsConfig
  , ledger :: LedgerConfig
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data FintsConfig = FintsConfig
  { account :: String
  , blz :: String
  , endpoint :: String
  , selectedAccount :: Maybe String
  , password :: Password
  }
  deriving (Show, Generic)

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
  deriving (Show, Generic, FromJSON, ToJSON)

data Filling = Filling
  { match :: Match
  , fill :: Fill
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- a Map from a field to a default value for this field
type DefaultsConfig = Map Text Text

-- a Map from the field to a pattern to match on
type Match = Map Text Text

-- a Map from the field to fill to the content that should be filled in
-- an empty filling means that this field should still be prompted for
type Fill = Map Text (Maybe Text)

newtype Password = Password {getPassword :: String}
  deriving (Generic)

instance FromJSON Password where
  parseJSON = fmap Password . parseJSON

instance ToJSON Password where
  toJSON value = toJSON value.getPassword

instance Show Password where
  show = const "********"

getYamlConfig :: ConfigDirectory -> IO YamlConfig
getYamlConfig configDirectory = decodeFileThrow $ getConfigFilePath configDirectory

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
          { prompts = ["creditAccount"]
          , defaults = fromList [("debitAccount", "assets:bank:checking")]
          , md5 = ["date", "payee", "purpose", "amount"]
          , fills = []
          }
    }
