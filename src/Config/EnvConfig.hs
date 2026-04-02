module Config.EnvConfig (EnvConfig (..), parseEnvConfig) where

import Config.YamlConfig (Password (..))
import Data.Text qualified as T (pack)
import System.Environment (lookupEnv)

data EnvConfig = EnvConfig
  { password :: Maybe Password
  }

parseEnvConfig :: IO EnvConfig
parseEnvConfig = do
  password <- lookupEnv "FINTS_PASSWORD"
  return EnvConfig{password = Password . T.pack <$> password}
