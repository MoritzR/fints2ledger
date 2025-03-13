module Main (main) where

import Test.Syd
import qualified ConfigSpec
import qualified Dates.ParserSpec
import qualified Matching.MatchingSpec
import qualified Matching.ParserSpec
import qualified PromptSpec
import qualified TransactionSpec
import qualified UtilsSpec

main :: IO ()
main = sydTest $ describe "fints2ledger" do
  ConfigSpec.spec
  Dates.ParserSpec.spec
  Matching.MatchingSpec.spec
  Matching.ParserSpec.spec
  PromptSpec.spec
  TransactionSpec.spec
  UtilsSpec.spec
