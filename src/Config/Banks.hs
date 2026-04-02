module Config.Banks (Bank (..), fintsEndpoint) where

data Bank = ING | GLS | OTHER deriving (Eq, Show, Enum, Bounded)

fintsEndpoint :: Bank -> String
fintsEndpoint = \case
  ING -> "https://fints.ing.de/fints"
  GLS -> "https://fints1.atruvia.de/cgi-bin/hbciservlet"
  OTHER -> ""
