{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Transactions (
  getTransactionsFromFinTS,
  getExampleTransactions,
  getTransactionsFromCsv,
  writeTransactionsToCsv,
  transactionsToCsv,
  Transaction (..),
  Amount (..),
)
where

import Config.AppConfig (AppConfig (..))
import Config.Files (ConfigDirectory (..), exampleFile, pyfintsFile)
import Config.YamlConfig (FintsConfig (..), Password (..))
import Control.Exception (Exception, throwIO)
import Control.Monad (forM_)
import Data.Aeson (FromJSON, ToJSON, (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString qualified as StrictBS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as SBS
import Data.ByteString.Lazy qualified as BS
import Data.Csv (DefaultOrdered, FromField, FromNamedRecord, ToField, ToNamedRecord)
import Data.Csv qualified as Csv
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time (Day, defaultTimeLocale, formatTime)
import Data.Vector (toList)
import GHC.Generics (Generic)
import Hledger (getCurrentDay)
import System.Console.Haskeline qualified as Haskeline
import System.Exit (ExitCode (..))
import System.IO (Handle, hFlush, hIsEOF)
import System.IO.Temp (withSystemTempFile)
import System.Process.Typed (createPipe, getStdin, getStdout, inherit, proc, setStderr, setStdin, setStdout, waitExitCode, withProcessWait)
import Utils (formatDouble, orElseThrow, (??))

getExampleTransactions :: IO [Transaction]
getExampleTransactions = do
  decodeTransactions exampleFile `orElseThrow` TransactionDecodeError
 where
  decodeTransactions = Aeson.eitherDecode . TL.encodeUtf8 . TL.fromStrict

getTransactionsFromCsv :: FilePath -> IO [Transaction]
getTransactionsFromCsv path = do
  csvContents <- BS.readFile path
  case Csv.decodeByName csvContents of
    Right (_header, rows) -> return $ toList rows
    Left message -> throwIO $ CsvDecodeError message

writeTransactionsToCsv :: FilePath -> [Transaction] -> IO ()
writeTransactionsToCsv path = BS.writeFile path . transactionsToCsv

transactionsToCsv :: [Transaction] -> BS.ByteString
transactionsToCsv = Csv.encodeDefaultOrderedByNameWith Csv.defaultEncodeOptions{Csv.encUseCrLf = False}

getTransactionsFromFinTS :: AppConfig -> IO [Transaction]
getTransactionsFromFinTS config = do
  currentDay <- getCurrentDay
  password <- maybe getPassword return config.fintsConfig.password

  let pyfintsArgs =
        PyFintsArguments
          { account = T.unpack config.fintsConfig.account
          , blz = T.unpack config.fintsConfig.blz
          , endpoint = T.unpack config.fintsConfig.endpoint
          , selectedAccount = T.unpack $ config.fintsConfig.selectedAccount ?? config.fintsConfig.account
          , password = password
          , start = formatDayForPython config.startDate
          , end = formatDayForPython currentDay
          , stateDirectory = config.configDirectory.get
          }

  withSystemTempFile "fints2ledger.py" \path handle -> do
    TIO.hPutStr handle pyfintsFile
    hFlush handle

    let processConfig =
          proc config.pythonExecutable [path]
            & setStdin createPipe
            & setStdout createPipe
            & setStderr inherit
    withProcessWait processConfig \process -> do
      sendMessage (getStdin process) $ Aeson.object ["type" Aeson..= ("start" :: Text), "arguments" Aeson..= pyfintsArgs]
      result <- handleMessages (getStdin process) (getStdout process)
      exitCode <- waitExitCode process
      case (exitCode, result) of
        (ExitSuccess, Right transactions) -> return transactions
        (_, Left message) -> throwIO $ PyFintsError $ T.unpack message
        (ExitFailure _, Right _) -> throwIO $ PyFintsError "The FinTS process failed, check the message above."

sendMessage :: (ToJSON message) => Handle -> message -> IO ()
sendMessage handle message = do
  BS.hPutStr handle $ Aeson.encode message <> "\n"
  hFlush handle

handleMessages :: Handle -> Handle -> IO (Either Text [Transaction])
handleMessages input output = do
  eof <- hIsEOF output
  if eof
    then return $ Left "The FinTS process ended without returning transactions."
    else do
      line <- SBS.hGetLine output
      case Aeson.eitherDecodeStrict line of
        Left message -> throwIO $ PyFintsError $ "Invalid message from FinTS process: " <> message
        Right workerMessage -> case workerMessage of
          TanMethods methods -> do
            selected <- promptForChoice "TAN method" (map (\method -> (method.identifier, method.name)) methods)
            sendMessage input $ Aeson.object ["type" Aeson..= ("tan_method" :: Text), "id" Aeson..= selected]
            handleMessages input output
          TanMedia media -> do
            selected <- promptForChoice "TAN medium" (map (\medium -> (medium.index, mediumLabel medium)) media)
            sendMessage input $ Aeson.object ["type" Aeson..= ("tan_medium" :: Text), "index" Aeson..= selected]
            handleMessages input output
          TanChallenge challenge -> do
            tanValue <- promptForTan challenge
            sendMessage input $ Aeson.object ["type" Aeson..= ("tan" :: Text), "value" Aeson..= tanValue]
            handleMessages input output
          Transactions transactions -> return $ Right transactions
          WorkerError message -> return $ Left message

promptForTan :: TanChallengeDetails -> IO Text
promptForTan challenge = do
  TIO.putStrLn challenge.message
  case challenge.hhduc of
    Just hhduc -> TIO.putStrLn $ "chipTAN challenge data: " <> hhduc
    Nothing -> return ()
  case challenge.matrix of
    Just matrix -> withPhotoTanFile matrix askForTan
    Nothing -> askForTan
 where
  askForTan
    | challenge.decoupled = do
        Haskeline.runInputT Haskeline.defaultSettings $ Haskeline.getInputLine "Approve the request in your banking app, then press Enter to check again."
        return ""
    | otherwise = do
        Password tanValue <- getSecret "TAN: "
        return tanValue

withPhotoTanFile :: TanMatrix -> IO value -> IO value
withPhotoTanFile matrix action =
  withSystemTempFile ("fints2ledger-photo-tan" <> matrixExtension matrix.mimeType) \path handle -> do
    case Base16.decode $ T.encodeUtf8 matrix.encodedData of
      Left message -> throwIO $ PyFintsError $ "Invalid photoTAN image: " <> message
      Right image -> StrictBS.hPut handle image
    hFlush handle
    TIO.putStrLn $ "photoTAN image: " <> T.pack path
    action

matrixExtension :: Text -> String
matrixExtension "image/png" = ".png"
matrixExtension "image/jpeg" = ".jpg"
matrixExtension _ = ".img"

promptForChoice :: Text -> [(identifier, Text)] -> IO identifier
promptForChoice label choices = do
  TIO.putStrLn $ "Available " <> label <> "s:"
  forM_ (zip [(1 :: Int) ..] choices) \(number, (_, description)) ->
    TIO.putStrLn $ "  " <> T.pack (show number) <> ". " <> description
  Haskeline.runInputT Haskeline.defaultSettings ask
 where
  ask = do
    answer <- Haskeline.getInputLine $ T.unpack $ label <> " (number): "
    case answer >>= readChoice of
      Just choice -> return $ fst $ choices !! (choice - 1)
      Nothing -> Haskeline.outputStrLn "Please enter one of the numbers shown above." >> ask
  readChoice value = case reads value of
    [(choice, "")] | choice >= 1 && choice <= length choices -> Just choice
    _ -> Nothing

mediumLabel :: TanMedium -> Text
mediumLabel medium = medium.name <> maybe "" (\mobile -> " (" <> mobile <> ")") medium.mobile

getPassword :: IO Password
getPassword = getSecret "Banking Password: "

getSecret :: String -> IO Password
getSecret prompt = do
  Haskeline.runInputT Haskeline.defaultSettings do
    maybePassword <- Haskeline.getPassword (Just '*') prompt
    Haskeline.outputStrLn ""
    return $ Password $ T.pack (maybePassword ?? "")

formatDayForPython :: Day -> String
formatDayForPython = formatTime defaultTimeLocale "%Y/%m/%d"

data PyFintsArguments = PyFintsArguments
  { account :: String
  , blz :: String
  , endpoint :: String
  , selectedAccount :: String
  , password :: Password
  , start :: String
  , end :: String
  , stateDirectory :: FilePath
  }
  deriving (Generic, ToJSON)

data TanMethod = TanMethod {identifier :: Text, name :: Text}
data TanMedium = TanMedium {index :: Int, name :: Text, mobile :: Maybe Text}
data TanMatrix = TanMatrix {mimeType :: Text, encodedData :: Text}
data TanChallengeDetails = TanChallengeDetails {message :: Text, decoupled :: Bool, hhduc :: Maybe Text, matrix :: Maybe TanMatrix}

data WorkerMessage
  = TanMethods [TanMethod]
  | TanMedia [TanMedium]
  | TanChallenge TanChallengeDetails
  | Transactions [Transaction]
  | WorkerError Text

instance FromJSON TanMethod where
  parseJSON = Aeson.withObject "TAN method" \object -> TanMethod <$> object .: "id" <*> object .: "name"

instance FromJSON TanMedium where
  parseJSON = Aeson.withObject "TAN medium" \object -> TanMedium <$> object .: "index" <*> object .: "name" <*> object .: "mobile"

instance FromJSON TanMatrix where
  parseJSON = Aeson.withObject "TAN matrix" \object -> TanMatrix <$> object .: "mimeType" <*> object .: "data"

instance FromJSON TanChallengeDetails where
  parseJSON = Aeson.withObject "TAN challenge" \object -> TanChallengeDetails <$> object .: "challenge" <*> object .: "decoupled" <*> object .: "hhduc" <*> object .: "matrix"

instance FromJSON WorkerMessage where
  parseJSON = Aeson.withObject "FinTS worker message" \object -> do
    messageType <- object .: "type" :: Aeson.Parser Text
    case messageType of
      "tan_methods" -> TanMethods <$> object .: "methods"
      "tan_media" -> TanMedia <$> object .: "media"
      "tan_challenge" -> TanChallenge <$> object .: "challenge"
      "transactions" -> Transactions <$> object .: "transactions"
      "error" -> WorkerError <$> object .: "message"
      unknown -> fail $ "Unknown FinTS worker message: " <> T.unpack unknown

newtype Amount = Amount {amount :: Double}
  deriving newtype (Num, Show, Eq, FromField)

instance ToField Amount where
  toField amount = T.encodeUtf8 $ formatDouble amount.amount

instance FromJSON Amount where
  parseJSON value = Amount . read <$> Aeson.parseJSON value

instance ToJSON Amount where
  toJSON value = Aeson.toJSON $ formatDouble value.amount

data Transaction = Transaction
  { date :: Text
  , amount :: Amount
  , currency :: Text
  , posting :: Text
  , payee :: Text
  , purpose :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, Eq, DefaultOrdered)

instance FromNamedRecord Transaction
instance ToNamedRecord Transaction

newtype TransactionDecodeError = TransactionDecodeError String deriving (Show)

instance Exception TransactionDecodeError

newtype PyFintsError = PyFintsError String deriving (Show)

instance Exception PyFintsError

newtype CsvDecodeError = CsvDecodeError String deriving (Show)

instance Exception CsvDecodeError
