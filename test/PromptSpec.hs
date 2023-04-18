module PromptSpec (spec) where

import App (Env (..), PromptResult (Skip))
import Config.AppConfig (AppConfig (..))
import Config.YamlConfig (LedgerConfig (..))
import Control.Monad (join)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Map (empty, fromList)
import Data.Time.Calendar (Day (ModifiedJulianDay))
import Prompt (transactionsToLedger)
import Test.Syd (Spec, describe, it, shouldContain)
import Transactions (Amount (Amount), Transaction (..))

testConfig :: AppConfig
testConfig =
  Config
    { isDemo = True
    , journalFile = "testJournal.ledger"
    , startDate = ModifiedJulianDay 0
    , configDirectory = "testConfigDirectory"
    , fintsConfig = error "Tests should need a fintsConfig"
    , ledgerConfig =
        LedgerConfig
          { defaults = empty
          , md5 = []
          , prompts = []
          , fills = []
          }
    }

spec :: Spec
spec = do
  describe "Prompt" do
    it "shows the transaction and the default values" do
      let transaction =
            Transaction
              { date = "01/01/2023"
              , amount = Amount 99.99
              , currency = "EUR"
              , posting = "test posting"
              , payee = "test payee"
              , purpose = "test purpose"
              }
      outputRef <- newIORef []
      let env =
            Env
              { config =
                  testConfig
                    { ledgerConfig =
                        testConfig.ledgerConfig
                          { defaults = fromList [("debitAccount", "assets:bank:checking")]
                          }
                    }
              , putStrLn = \s -> modifyIORef outputRef (++ [s])
              , promptForEntry = \_templateMap _key -> return Skip
              , readFile = const $ return ""
              , appendFile = \_filePath _text -> return ()
              }
      runReaderT (transactionsToLedger [transaction]) env
      output <- join <$> readIORef outputRef

      -- transaction values
      output `shouldContain` transaction.date
      output `shouldContain` "99.99"
      output `shouldContain` transaction.currency
      output `shouldContain` transaction.posting
      output `shouldContain` transaction.purpose
      output `shouldContain` "assets:bank:checking"