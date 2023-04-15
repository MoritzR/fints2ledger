module Prompt (transactionsToLedger) where

import Completion (runCompletion)
import Config.AppConfig (AppConfig (..))
import Config.Files (getTemplatePath)
import Config.YamlConfig (Fill, Filling (..), LedgerConfig (..))
import Control.Arrow ((>>>))
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Crypto.Hash.MD5 qualified as MD5
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Base16 qualified as Base16
import Data.Either (partitionEithers)
import Data.Function ((&))
import Data.Map (Map, fromList, insert, toList, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set (Set, notMember)
import Data.Set qualified as Set
import Data.String (IsString (fromString))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Format.Heavy (format)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TLIO
import GHC.Arr (Array, elems)
import Matching.Matching (findMatch)
import System.Console.Haskeline (getInputLine)
import Text.Regex.TDFA (AllTextMatches (getAllTextMatches), (=~))
import Transactions (Transaction (..))
import Utils (byteStringToString, printEmptyLine)

-- a Map of key/value pairs that will be used to fill the template file
type TemplateMap = Map Text Text

transactionsToLedger :: AppConfig -> [Transaction] -> IO ()
transactionsToLedger config transactions = do
  existingMd5Sums <- getExistingMd5Sums <$> TLIO.readFile config.journalFile
  template <- readFile $ getTemplatePath config.configDirectory

  forM_ transactions do
    transactionToLedger config existingMd5Sums template

insertTransaction :: Transaction -> TemplateMap -> TemplateMap
insertTransaction transaction =
  insert "date" (TL.pack transaction.date)
    >>> insert "amount" (TL.pack $ show transaction.amount)
    >>> insert "currency" (TL.pack transaction.currency)
    >>> insert "payee" (TL.pack transaction.payee)
    >>> insert "posting" (TL.pack transaction.posting)
    >>> insert "purpose" (TL.pack transaction.purpose)

insertCreditDebit :: Transaction -> TemplateMap -> TemplateMap
insertCreditDebit transaction =
  insert "debit" (TL.pack $ show transaction.amount)
    >>> insert "credit" (TL.pack $ show $ -transaction.amount)

getMd5 :: LedgerConfig -> TemplateMap -> Text
getMd5 ledgerConfig templateMap = TL.fromStrict md5
 where
  md5Keys = TL.pack <$> ledgerConfig.md5
  -- TODO throw a meaningful error instead of using fromJust or make this state impossible
  md5Values = fromJust . flip Map.lookup templateMap <$> md5Keys
  md5 = decodeUtf8 $ Base16.encode $ MD5.finalize $ foldl MD5.update MD5.init (encodeUtf8 . TL.toStrict <$> md5Values)

transactionToLedger :: AppConfig -> Set Text -> String -> Transaction -> IO ()
transactionToLedger config existingMd5Sums template transaction = do
  when (md5Sum `notMember` existingMd5Sums) do
    maybeResult <- prompter templateMapToShow
    case maybeResult of
      Skip -> return ()
      Result templateMapWithUserInputs -> do
        let templateMapFinal =
              templateMapWithUserInputs
                & insert "md5sum" md5Sum
                & insertCreditDebit transaction
        let renderedTemplate = format (fromString template) templateMapFinal
        TLIO.appendFile config.journalFile $ "\n\n" <> renderedTemplate
 where
  templateMapToShow = insertTransaction transaction config.ledgerConfig.defaults
  md5Sum = getMd5 config.ledgerConfig templateMapToShow
  prompter = case findMatch templateMapToShow config.ledgerConfig.fills of
    Just filling -> getPromptResultForMatchingEntry config filling.fill
    Nothing -> getPromptResultForManualEntry config

getPromptResultForMatchingEntry :: AppConfig -> Fill -> TemplateMap -> IO (PromptResult TemplateMap)
getPromptResultForMatchingEntry config fill templateMap = do
  let fillAsList = toList fill
  let (prompts, fills) =
        fillAsList
          & map
            ( \(key, maybeValue) ->
                case maybeValue of
                  Just value -> Right (key, value)
                  Nothing -> Left key
            )
          & partitionEithers
  -- TODO add a small delay and some output so that one can see what is filled in

  let templateMapWithFills = fromList fills <> templateMap

  printTemplateMap templateMapWithFills
  updateTemplateMapFromPrompts config prompts templateMapWithFills

getPromptResultForManualEntry :: AppConfig -> TemplateMap -> IO (PromptResult TemplateMap)
getPromptResultForManualEntry config templateMap = do
  printTemplateMap templateMap
  updateTemplateMapFromPrompts config config.ledgerConfig.prompts templateMap

updateTemplateMapFromPrompts :: AppConfig -> [Text] -> TemplateMap -> IO (PromptResult TemplateMap)
updateTemplateMapFromPrompts _config [] templateMap = return $ Result templateMap
updateTemplateMapFromPrompts config (prompt : restPrompts) templateMap = do
  maybeResult <- promptForEntry config templateMap prompt
  case maybeResult of
    Result result -> do
      updateTemplateMapFromPrompts config restPrompts $ insert prompt result templateMap
    -- TODO use fmap instead
    Skip -> return Skip

promptForEntry :: AppConfig -> TemplateMap -> Text -> IO (PromptResult Text)
promptForEntry config templateMap key = runCompletion config key do
  liftIO printEmptyLine
  line <- getInputLine $ TL.unpack ("> " <> key <> ": ")
  case line of
    Nothing -> return Skip
    Just "s" -> return Skip
    -- TODO throw error instead? This can happen when the user doesn't provide an account and also sets no default
    Just "" -> return $ maybe Skip Result (templateMap !? key)
    Just value -> return $ Result $ TL.strip $ TL.pack value

md5Regex :: Text
md5Regex = "; md5sum: ([A-Za-z0-9]{32})"

getExistingMd5Sums :: Text -> Set Text
getExistingMd5Sums textToSearchIn =
  let regexMatch = getAllTextMatches $ textToSearchIn =~ md5Regex :: Array Int [Text]
   in elems regexMatch
        & map (!! 1)
        & Set.fromList

printTemplateMap :: TemplateMap -> IO ()
printTemplateMap templateMap = do
  printEmptyLine
  printEmptyLine
  putStrLn $ byteStringToString $ encodePretty templateMap

data PromptResult a = Result a | Skip deriving (Show)
