{-# LANGUAGE DataKinds #-}

module App (App, HasConfig) where

import Config.AppConfig (AppConfig)
import Control.Monad.Trans.Reader (ReaderT)
import GHC.Records (HasField)

type App env a = ReaderT env IO a

type HasConfig env = HasField "config" env AppConfig