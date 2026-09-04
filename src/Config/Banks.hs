module Config.Banks (Bank (..), fintsEndpoint) where

import Data.Text (Text)

data Bank = ING | GLS | DKB | OTHER deriving (Eq, Show, Enum, Bounded)

fintsEndpoint :: Bank -> Text
fintsEndpoint = \case
  ING -> "https://fints.ing.de/fints"
  GLS -> "https://fints1.atruvia.de/cgi-bin/hbciservlet"
  DKB -> "https://fints.dkb.de/fints"
  OTHER -> ""
