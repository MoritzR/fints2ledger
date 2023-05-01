module UI.ConfigFields (Fields (..)) where

data Fields = Account | Blz | Endpoint | Password | Defaults | Md5 | Prompts | Fills deriving (Eq, Ord, Show)
