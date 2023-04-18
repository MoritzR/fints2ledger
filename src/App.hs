{-# LANGUAGE DataKinds #-}

module App (App, PromptResult (..), Env (..)) where

import Config.AppConfig (AppConfig)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Map (Map)
import Data.Text.Lazy (Text)

type App a = ReaderT Env IO a

data Env = Env
  { config :: AppConfig
  , putStrLn :: String -> IO ()
  , readFile :: FilePath -> IO Text
  , appendFile :: FilePath -> Text -> IO ()
  , promptForEntry :: Map Text Text -> Text -> App (PromptResult Text)
  }

data PromptResult a = Result a | Skip deriving (Show)
