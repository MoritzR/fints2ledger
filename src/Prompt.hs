module Prompt (prompt) where

import Completion (runWithAccountCompletion, runWithNoCompletion)
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
import Data.List (find)
import Data.Map (Map, fromList, insert, toList, (!), (!?))
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
import Matching.Parser (amountParser, runParser)
import System.Console.Haskeline (InputT, getInputLine)
import Text.Regex.TDFA (AllTextMatches (getAllTextMatches), (=~))
import Transactions (Transaction (..))
import Utils (byteStringToString, printEmptyLine, (??))

-- a Map of key/value pairs that will be used to fill the template file
type TemplateMap = Map Text Text

prompt :: AppConfig -> [Transaction] -> IO ()
prompt config transactions = do
  existingMd5Sums <- getExistingMd5Sums <$> TLIO.readFile config.journalFile

  forM_ transactions do
    promptForTransaction existingMd5Sums config

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

promptForTransaction :: Set Text -> AppConfig -> Transaction -> IO ()
promptForTransaction existingMd5Sums config transaction = do
  let templateMapToShow =
        config.ledgerConfig.defaults
          & insertTransaction transaction

  let md5Sum = getMd5 config.ledgerConfig templateMapToShow

  when (md5Sum `notMember` existingMd5Sums) do
    let templateMapWithMd5AndDebit =
          templateMapToShow
            & insert "md5sum" md5Sum
            & insertCreditDebit transaction
    let prompter = case findMatch config.ledgerConfig.fills templateMapToShow of
          Just filling -> promptForMatchingEntry config filling.fill
          Nothing -> promptForManualEntry config config.ledgerConfig.prompts
    templateMapWithUserInputs <- prompter templateMapWithMd5AndDebit

    template <- readFile $ getTemplatePath config.configDirectory
    let renderedTemplate = format (fromString template) templateMapWithUserInputs
    TLIO.appendFile config.journalFile $ "\n\n" <> renderedTemplate

promptForMatchingEntry :: AppConfig -> Fill -> TemplateMap -> IO TemplateMap
promptForMatchingEntry config fill templateMap = do
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

promptForManualEntry :: AppConfig -> [Text] -> TemplateMap -> IO TemplateMap
promptForManualEntry config prompts templateMap = do
  printTemplateMap templateMap
  updateTemplateMapFromPrompts config prompts templateMap

updateTemplateMapFromPrompts :: AppConfig -> [Text] -> TemplateMap -> IO TemplateMap
updateTemplateMapFromPrompts config prompts templateMap = do
  results <- mapM (promptForTemplateMap config templateMap) prompts
  return $ fromList (zip prompts results) <> templateMap

findMatch :: [Filling] -> TemplateMap -> Maybe Filling
findMatch fillings templateMap =
  fillings
    & find (matches templateMap)

matches :: TemplateMap -> Filling -> Bool
matches templateMap filling =
  -- TODO check if this (!) can fail
  all (matchesOneEntry templateMap) (toList filling.match)

matchesOneEntry :: TemplateMap -> (Text, Text) -> Bool
matchesOneEntry templateMap (key, value) = case key of
  "amount" ->
    let
      matchesAmount = runParser amountParser (TL.unpack key) ?? const False
      amount = (read $ TL.unpack $ templateMap ! key)
     in
      matchesAmount amount
  _ -> value =~ (templateMap ! key)

runCompletionFor :: AppConfig -> Text -> (forall a. InputT IO a -> IO a)
runCompletionFor config key
  -- make this elem check more robust to renames
  | key `elem` ["creditAccount", "debitAccount"] = runWithAccountCompletion config
  | otherwise = runWithNoCompletion

promptForTemplateMap :: AppConfig -> TemplateMap -> Text -> IO Text
promptForTemplateMap config templateMap key = runCompletionFor config key do
  liftIO printEmptyLine
  line <- getInputLine $ TL.unpack (key <> ": ")
  case line of
    Nothing -> return "" -- TODO end program
    -- TODO throw error instead? This can happen when the user doesn't provide an account and also sets no default
    Just "" -> return $ templateMap !? key ?? "MISSING"
    Just value -> return $ TL.strip $ TL.pack value

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
