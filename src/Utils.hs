module Utils (encodeAsString, toLazyTemplateMap, byteStringToString, (??), orElseThrow, createFile, calculateMd5Value, formatDouble) where

import Control.Exception (Exception, throwIO)
import Crypto.Hash.MD5 qualified as MD5
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy (ByteString)
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL

byteStringToString :: ByteString -> String
byteStringToString = TL.unpack . TL.decodeUtf8

encodeAsString :: (ToJSON a) => a -> String
encodeAsString = byteStringToString . encode

(??) :: Maybe a -> a -> a
Nothing ?? a = a
Just a ?? _ = a

orElseThrow :: (Exception e) => Either String a -> (String -> e) -> IO a
orElseThrow eitherAB toException = case eitherAB of
  Left err -> throwIO $ toException err
  Right value -> return value

createFile :: FilePath -> IO ()
createFile path = writeFile path ""

calculateMd5Value :: [Text] -> Text
calculateMd5Value md5Values =
  map textToByteString md5Values
    & foldl MD5.update MD5.init
    & MD5.finalize
    & byteStringToText
 where
  textToByteString = T.encodeUtf8
  byteStringToText = T.decodeUtf8 . Base16.encode

formatDouble :: Double -> Text
formatDouble double
  | fromInteger floored == double = T.pack $ show floored
  | otherwise = T.pack $ show double
 where
  floored = floor double :: Integer

toLazyTemplateMap :: Map Text Text -> Map TL.Text TL.Text
toLazyTemplateMap templateMap =
  foldr
    (\(key, value) -> Map.insert (TL.fromStrict key) (TL.fromStrict value))
    Map.empty
    (Map.toList templateMap)
