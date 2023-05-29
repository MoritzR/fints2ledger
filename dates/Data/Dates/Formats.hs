{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, LambdaCase, QuasiQuotes, TemplateHaskell #-}
-- | This module allows to parse arbitrary date formats.
-- Date formats are specified as strings:
--
--  * "DD.MM.YYY"
--
--  * "YYYY\/MM\/DD"
--
--  * "DD\/MM\/YYYY, HH:mm:SS"
--
--  * "YY.MM.DD[, HH:mm:SS]"
--
--  * and so on.
--
module Data.Dates.Formats
  (FormatElement (..), Format,
   FormatParser,
   parseFormat, pFormat, formatParser,
   parseDateFormat,
   df
  ) where

import Control.Applicative ((<$>))
import Data.Monoid
import Text.Parsec
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.Dates.Types
import Data.Dates.Internal (number)

-- | Date\/time format element
data FormatElement =
    YEAR   Bool Int
  | MONTH  Bool Int
  | DAY    Bool Int
  | HOUR   Bool Int
  | MINUTE Bool Int
  | SECOND Bool Int
  | Whitespace Bool
  | Fixed  Bool String
  deriving (Eq, Show)

type FormatParser a = Parsec String Bool a

-- | Date\/time format
type Format = [FormatElement]

nchars ∷ Char → FormatParser Int
nchars c = do
  s ← many1 $ char c
  return $ length s

brackets :: FormatParser a -> FormatParser a
brackets p = do
  char '['
  setState False
  result <- p
  char ']'
  setState True
  return result

pFormat :: FormatParser Format
pFormat = do
    elems <- many1 $ try (brackets format) <|> format
    return $ concat elems

  where
    format :: FormatParser Format
    format =
      many1 $ choice $ map try [element YEAR 'Y', element MONTH 'M',
                                         element DAY  'D', element HOUR  'H',
                                         element MINUTE 'm', element SECOND 'S',
                                         whitespaces, fixed]

    element constr c = do
      mandatory <- getState
      constr mandatory <$> nchars c

    whitespaces = do
      many1 $ oneOf " \r\n\t"
      mandatory <- getState
      return $ Whitespace mandatory

    fixed = do
      mandatory <- getState
      Fixed mandatory <$> (many1 $ noneOf "YMDHmS[] \t\r\n")

pYear ∷ Stream s m Char => Int → ParsecT s st m DateTime
pYear n = do
  y ← number n 10000
  if y < 2000
    then return $ mempty {year = y+2000}
    else return $ mempty {year = y}

pMonth ∷ Stream s m Char => Int → ParsecT s st m DateTime
pMonth n = do
  m ← number n 12
  return $ mempty {month = m}

pDay ∷ Stream s m Char => Int → ParsecT s st m DateTime
pDay n = do
  d ← number n 31
  return $ mempty {day = d}

pHour ∷ Stream s m Char => Int → ParsecT s st m DateTime
pHour n = do
  h ← number n 23
  return $ mempty {hour = h}

pMinute ∷ Stream s m Char => Int → ParsecT s st m DateTime
pMinute n = do
  m ← number n 59
  return $ mempty {minute = m}

pSecond ∷ Stream s m Char => Int → ParsecT s st m DateTime
pSecond n = do
  s ← number n 59
  return $ mempty {second = s}

opt :: Stream s m Char => Monoid a => Bool -> ParsecT s st m a -> ParsecT s st m a
opt True  p = p
opt False p = option mempty p

parseFormat :: String -> Either ParseError Format
parseFormat formatStr = runParser pFormat True "(date format string)" formatStr

-- | Make Parser for specified date format.
formatParser ∷ Stream s m Char => Format → ParsecT s st m DateTime
formatParser format = mconcat <$> mapM parser format
  where
    parser (YEAR   m n) = opt m $ pYear n
    parser (MONTH  m n) = opt m $ pMonth n
    parser (DAY    m n) = opt m $ pDay n
    parser (HOUR   m n) = opt m $ pHour n
    parser (MINUTE m n) = opt m $ pMinute n
    parser (SECOND m n) = opt m $ pSecond n
    parser (Whitespace m) = opt m ((many1 $ oneOf " \t\r\n") >> return mempty)
    parser (Fixed  m s) = opt m ( string s >> return mempty )

-- | Parse date\/time in specified format.
parseDateFormat :: String  -- ^ Format string, i.e. "DD.MM.YY"
                -> String  -- ^ String to parse
                -> Either ParseError DateTime
parseDateFormat formatStr str = do
  format <- parseFormat formatStr 
  runParser (formatParser format) () "(date)" str

df :: QuasiQuoter
df = QuasiQuoter
  { quoteExp = either (fail . show) (listE . fmap fe2th) . parseFormat
  , quotePat = err
  , quoteType = err
  , quoteDec = err
  }
  where
  fe2th :: FormatElement -> ExpQ
  fe2th = \case
    YEAR       x y -> [e|YEAR       $(b x) $(i y)      |]
    MONTH      x y -> [e|MONTH      $(b x) $(i y)      |]
    DAY        x y -> [e|DAY        $(b x) $(i y)      |]
    HOUR       x y -> [e|HOUR       $(b x) $(i y)      |]
    MINUTE     x y -> [e|MINUTE     $(b x) $(i y)      |]
    SECOND     x y -> [e|SECOND     $(b x) $(i y)      |]
    Whitespace x   -> [e|Whitespace $(b x)             |]
    Fixed      x y -> [e|Fixed      $(b x) $(stringE y)|]
  err = const $ fail "Date format parser only defined for Exp"
  b :: Bool -> ExpQ
  b = conE . \case
    True  -> 'True
    False -> 'False
  i :: Int -> ExpQ
  i = litE . integerL . toInteger

