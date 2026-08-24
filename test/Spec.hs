module Main (main) where

import ConfigSpec qualified
import Dates.ParserSpec qualified
import Matching.MatchingSpec qualified
import Matching.ParserSpec qualified
import PromptSpec qualified
import Test.Syd
import TransactionSpec qualified
import UtilsSpec qualified

main :: IO ()
main = sydTest $ describe "fints2ledger" do
  ConfigSpec.spec
  Dates.ParserSpec.spec
  Matching.MatchingSpec.spec
  Matching.ParserSpec.spec
  PromptSpec.spec
  TransactionSpec.spec
  UtilsSpec.spec
