module UI.ConfigFields (Field (..)) where

data Field = Account | Blz | Endpoint | Password | JournalFile | Defaults | Md5 | Prompts | Fills deriving (Eq, Ord, Show)
