module UI.ConfigFields (Field (..), account, blz, password, endpoint, journalFile) where

import Data.Text (Text)

data Field = Field {label :: Text, helpText :: Text} deriving (Ord, Eq, Show)

account =
  Field
    { label = "Account"
    , helpText = "The account number you use to log into your banking account."
    }
blz =
  Field
    { label = "BLZ"
    , helpText = "Your banks BLZ"
    }
password =
  Field
    { label = "Password"
    , helpText = "Your banking password.\nLeave empty if you don't want to store it."
    }
endpoint =
  Field
    { label = "FinTS Endpoint"
    , helpText = "Your banks FinTS endpoint.\nFor example for ING this is: https://fints.ing.de/fints"
    }
journalFile =
  Field
    { label = "Journal File"
    , helpText = "The path to the ledger file where the transactions should be stored.\nFor example 'journal.ledger' (the default) or '~/journal.ledger'"
    }
