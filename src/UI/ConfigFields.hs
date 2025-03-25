module UI.ConfigFields (Field (..)) where

data Field = Account | Blz | Endpoint | Password | JournalFile deriving (Eq, Ord, Show)
