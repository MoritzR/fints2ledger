{-# LANGUAGE DataKinds #-}

module App (App, PromptResult (..), Env (..), printText) where

import Config.AppConfig (AppConfig)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks)
import Data.Map (Map)
import Data.Text (Text)
import Prelude hiding (putStrLn)

type App a = ReaderT Env IO a

{- | The environment defines the Apps dependencies.
  This is useful for testing, as these dependencies
  can be mocked easily. Apart from the AppConfig,
  it contains all the IO functions, like printing to
  the terminal or reading a file.
-}
data Env = Env
  { config :: AppConfig
  , putStrLn :: Text -> IO ()
  , readFile :: FilePath -> IO Text
  , appendFile :: FilePath -> Text -> IO ()
  , promptForEntry :: Map Text Text -> Text -> App (PromptResult Text)
  , sleep :: IO ()
  }

data PromptResult a = Result a | Skip deriving (Show)

printText :: Text -> App ()
printText text = do
  putStrLn <- asks (.putStrLn)
  liftIO $ putStrLn text
