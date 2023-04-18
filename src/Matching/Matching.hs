module Matching.Matching (findMatch) where

import Config.YamlConfig (Filling (..))
import Data.Foldable (find)
import Data.Map (Map, toList, (!))
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Matching.Parser (amountParser, runParser)
import Text.Regex.TDFA ((=~))
import Utils ((??))

findMatch :: Map Text Text -> [Filling] -> Maybe Filling
findMatch templateMap = find $ matches templateMap

matches :: Map Text Text -> Filling -> Bool
matches templateMap filling =
  all matchesEntry $ toList filling.match
 where
  matchesEntry (key, value) = case key of
    -- TODO make this string more safe
    "amount" ->
      let
        matchesAmount = runParser amountParser (TL.unpack key) ?? const False
        -- TODO check if this (!) can fail
        amount = read $ TL.unpack $ templateMap ! key
       in
        matchesAmount amount
    _ -> value =~ (templateMap ! key)
