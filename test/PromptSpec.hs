module PromptSpec (spec) where

import App (Env (..), PromptResult (Result, Skip))
import Config.AppConfig (AppConfig (..))
import Config.YamlConfig (Filling (Filling, match), LedgerConfig (..), fill)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Map (empty, fromList)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Calendar (Day (ModifiedJulianDay))
import Prompt (transactionsToLedger)
import Test.Syd (Spec, describe, goldenTextFile, it, shouldBe, shouldContain)
import Transactions (Amount (Amount), Transaction (..))

spec :: Spec
spec = do
  describe "transactionsToLedger" do
    it "shows the transaction" do
      output <- runToLedger [testTransaction] testEnv

      output `shouldContain` T.unpack testTransaction.date
      output `shouldContain` "99.99"
      output `shouldContain` T.unpack testTransaction.currency
      output `shouldContain` T.unpack testTransaction.posting
      output `shouldContain` T.unpack testTransaction.purpose

    it "shows the default values" do
      let env =
            testEnv
              { config =
                  testConfig
                    { ledgerConfig =
                        testConfig.ledgerConfig
                          { defaults = fromList [("debit_account", "assets:bank:checking")]
                          }
                    }
              }
      output <- runToLedger [testTransaction] env
      output `shouldContain` "assets:bank:checking"

    it "shows the automatically filled in values" do
      let env =
            testEnv
              { config =
                  testConfig
                    { ledgerConfig =
                        testConfig.ledgerConfig
                          { fills =
                              [ Filling
                                  { match = fromList [("payee", ".*")]
                                  , fill = fromList [("purpose", Just "automatically filled in purpose")]
                                  }
                              ]
                          }
                    }
              }
      output <- runToLedger [testTransaction] env
      output `shouldContain` "automatically filled in purpose"

    it "skips entries that have their md5 already in the journal" do
      ioRef <- newIORef []
      let env =
            testEnv
              { readFile = const $ return "; md5sum: 5abd61b9de15c3115519a5f1b4ac7992"
              , promptForEntry = \_templateMap key -> do
                  liftIO $ modifyIORef ioRef (++ [key])
                  return $ Result "test input"
              }
      runReaderT (transactionsToLedger [testTransaction]) env
      promptedFor <- readIORef ioRef

      promptedFor `shouldBe` []

    it "doesn't prompt for entries that are autofilled" do
      ioRef <- newIORef []
      let env =
            testEnv
              { config =
                  testConfig
                    { ledgerConfig =
                        testConfig.ledgerConfig
                          { fills =
                              [ Filling
                                  { match = fromList [("payee", ".*")]
                                  , fill = empty
                                  }
                              ]
                          }
                    }
              , promptForEntry = \_templateMap key -> do
                  liftIO $ modifyIORef ioRef (++ [key])
                  return $ Result "test input"
              }
      runReaderT (transactionsToLedger [testTransaction]) env
      promptedFor <- readIORef ioRef

      promptedFor `shouldBe` []

    it "prompts for entries that are autofilled but have an empty fill key" do
      ioRef <- newIORef []
      let env =
            testEnv
              { config =
                  testConfig
                    { ledgerConfig =
                        testConfig.ledgerConfig
                          { fills =
                              [ Filling
                                  { match = fromList [("payee", ".*")]
                                  , fill = fromList [("purpose", Nothing), ("payee", Nothing)]
                                  }
                              ]
                          }
                    }
              , promptForEntry = \_templateMap key -> do
                  liftIO $ modifyIORef ioRef (++ [key])
                  return $ Result "test input"
              }
      runReaderT (transactionsToLedger [testTransaction]) env
      promptedFor <- readIORef ioRef

      promptedFor `shouldBe` ["payee", "purpose"]

    it "matches the snapshot" do
      let getSnapshot = do
            ioRef <- newIORef ""
            let env =
                  testEnv
                    { promptForEntry = \_templateMap _key -> do
                        return $ Result "expenses:test"
                    , appendFile = \_filePath text -> modifyIORef ioRef (<> text)
                    }

            runToLedger [testTransaction, testTransaction{amount = Amount 0.01}] env
            readIORef ioRef

      goldenTextFile "test/files/snapshot.ledger" getSnapshot

testConfig :: AppConfig
testConfig =
  Config
    { journalFile = "testJournal.ledger"
    , startDate = ModifiedJulianDay 0
    , configDirectory = "testConfigDirectory"
    , fintsConfig = error "Tests should need a fintsConfig"
    , ledgerConfig =
        LedgerConfig
          { defaults = fromList [("debit_account", "assets:test")]
          , md5 = ["purpose"]
          , prompts = ["credit_account"]
          , fills = []
          }
    , pythonExecutable = "echo \"echo python\""
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
    , readFile = \path ->
        if "template.txt" `isInfixOf` path
          then TIO.readFile path
          else return ""
    , appendFile = \_filePath _text -> return ()
    , sleep = return ()
    }

runToLedger :: [Transaction] -> Env -> IO String
runToLedger transactions env = do
  ioRef <- newIORef []
  runReaderT
    (transactionsToLedger transactions)
    env
      { putStrLn = \s -> modifyIORef ioRef (++ [s])
      }
  T.unpack . T.concat <$> readIORef ioRef
