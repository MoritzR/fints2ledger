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

spec :: Spec
spec = do
  describe "transactionsToLedger" do
    it "shows the transaction and the default values" do
      let env =
            testEnv
              { config =
                  testConfig
                    { ledgerConfig =
                        testConfig.ledgerConfig
                          { defaults = fromList [("debitAccount", "assets:bank:checking")]
                          }
                    }
              }
      output <- runToLedger [testTransaction] env

      -- transaction values
      output `shouldContain` testTransaction.date
      output `shouldContain` "99.99"
      output `shouldContain` testTransaction.currency
      output `shouldContain` testTransaction.posting
      output `shouldContain` testTransaction.purpose

      -- default values
      output `shouldContain` "assets:bank:checking"

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

testTransaction :: Transaction
testTransaction =
  Transaction
    { date = "01/01/2023"
    , amount = Amount 99.99
    , currency = "EUR"
    , posting = "test posting"
    , payee = "test payee"
    , purpose = "test purpose"
    }

testEnv :: Env
testEnv =
  Env
    { config = testConfig
    , promptForEntry = \_templateMap _key -> return Skip
    , putStrLn = const $ return ()
    , readFile = const $ return ""
    , appendFile = \_filePath _text -> return ()
    }

runToLedger :: [Transaction] -> Env -> IO String
runToLedger transactions env = do
  ioRef <- newIORef []
  runReaderT
    (transactionsToLedger transactions)
    env
      { putStrLn = \s -> modifyIORef ioRef (++ [s])
      }
  join <$> readIORef ioRef