module Utils (encodeAsString, byteStringToString, (??), orElseThrow, createFile, calculateMd5Value, formatDouble) where

import Control.Exception (Exception, throwIO)
import Crypto.Hash.MD5 qualified as MD5
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy (ByteString)
import Data.Text.Encoding qualified as T
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL

byteStringToString :: ByteString -> String
byteStringToString = TL.unpack . TL.decodeUtf8

encodeAsString :: ToJSON a => a -> String
encodeAsString = byteStringToString . encode

(??) :: Maybe a -> a -> a
Nothing ?? a = a
Just a ?? _ = a

orElseThrow :: Exception e => Either String a -> (String -> e) -> IO a
orElseThrow eitherAB toException = case eitherAB of
  Left err -> throwIO $ toException err
  Right value -> return value

createFile :: FilePath -> IO ()
createFile path = writeFile path ""

calculateMd5Value :: [Text] -> Text
calculateMd5Value md5Values = TL.fromStrict $ T.decodeUtf8 $ Base16.encode $ MD5.finalize $ foldl MD5.update MD5.init (T.encodeUtf8 . TL.toStrict <$> md5Values)

formatDouble :: Double -> String
formatDouble double
  | fromInteger floored == double = show floored
  | otherwise = show double
 where
  floored = floor double :: Integer
