module Utils (encodeAsString, byteStringToString, printEmptyLine, (??), orElseThrow, createFile) where

import Control.Exception (Exception, throwIO)
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)

byteStringToString :: ByteString -> String
byteStringToString = unpack . decodeUtf8

encodeAsString :: ToJSON a => a -> String
encodeAsString = byteStringToString . encode

printEmptyLine :: IO ()
printEmptyLine = putStrLn ""

(??) :: Maybe a -> a -> a
Nothing ?? a = a
Just a ?? _ = a

orElseThrow :: Exception e => Either String a -> (String -> e) -> IO a
orElseThrow eitherAB toException = case eitherAB of
  Left err -> throwIO $ toException err
  Right value -> return value

createFile :: FilePath -> IO ()
createFile path = writeFile path ""
