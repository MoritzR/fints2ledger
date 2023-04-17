module Prompt (transactionsToLedger) where

import App (App, HasConfig)
import Completion (runCompletion)
import Config.AppConfig (AppConfig (..))
import Config.Files (getTemplatePath)
import Config.YamlConfig (Fill, Filling (..), LedgerConfig (..))
import Control.Arrow ((>>>))
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (asks)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Either (partitionEithers)
import Data.Function ((&))
import Data.Map (Map, fromList, insert, toList, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set (Set, notMember)
import Data.Set qualified as Set
import Data.String (IsString (fromString))
import Data.Text.Format.Heavy (format)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TLIO
import GHC.Arr (Array, elems)
import Matching.Matching (findMatch)
import System.Console.Haskeline (getInputLine)
import Text.Regex.TDFA (AllTextMatches (getAllTextMatches), (=~))
import Transactions (Amount (..), Transaction (..))
import Utils (byteStringToString, calculateMd5Value, formatDouble, printEmptyLine)

-- a Map of key/value pairs that will be used to fill the template file
type TemplateMap = Map Text Text

transactionsToLedger :: HasConfig env => [Transaction] -> App env ()
transactionsToLedger transactions = do
  config <- asks (.config)
  existingMd5Sums <- getExistingMd5Sums <$> liftIO (TLIO.readFile config.journalFile)
  template <- liftIO $ readFile $ getTemplatePath config.configDirectory
  forM_ transactions do
    transactionToLedger existingMd5Sums template

insertTransaction :: Transaction -> TemplateMap -> TemplateMap
insertTransaction transaction =
  insert "date" (TL.pack transaction.date)
    >>> insert "amount" (TL.pack $ formatDouble transaction.amount.amount)
    >>> insert "currency" (TL.pack transaction.currency)
    >>> insert "payee" (TL.pack transaction.payee)
    >>> insert "posting" (TL.pack transaction.posting)
    >>> insert "purpose" (TL.pack transaction.purpose)

insertCreditDebit :: Transaction -> TemplateMap -> TemplateMap
insertCreditDebit transaction =
  insert "debit" (TL.pack $ show transaction.amount)
    >>> insert "credit" (TL.pack $ show $ -transaction.amount)

getMd5 :: LedgerConfig -> TemplateMap -> Text
getMd5 ledgerConfig templateMap = calculateMd5Value md5Values
 where
  md5Keys = TL.pack <$> ledgerConfig.md5
  -- TODO throw a meaningful error instead of using fromJust or make this state impossible
  md5Values = fromJust . flip Map.lookup templateMap <$> md5Keys

transactionToLedger :: HasConfig env => Set Text -> String -> Transaction -> App env ()
transactionToLedger existingMd5Sums template transaction = do
  config <- asks (.config)
  let templateMapToShow = insertTransaction transaction config.ledgerConfig.defaults
      md5Sum = getMd5 config.ledgerConfig templateMapToShow
  when (md5Sum `notMember` existingMd5Sums) do
    let prompter = case findMatch templateMapToShow config.ledgerConfig.fills of
          Just filling -> getPromptResultForMatchingEntry config filling.fill
          Nothing -> getPromptResultForManualEntry config

    maybeResult <- liftIO $ prompter templateMapToShow
    case maybeResult of
      Skip -> return ()
      Result templateMapWithUserInputs -> do
        let templateMapFinal =
              templateMapWithUserInputs
                & insert "md5sum" md5Sum
                & insertCreditDebit transaction
        let renderedTemplate = format (fromString template) templateMapFinal
        liftIO $ TLIO.appendFile config.journalFile $ "\n\n" <> renderedTemplate

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
