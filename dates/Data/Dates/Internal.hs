{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable, FlexibleContexts #-}

module Data.Dates.Internal where

import Data.Char

import Text.Parsec

-- | Parser version of Prelude.read
tryRead :: (Read a, Stream s m Char) => String -> ParsecT s st m a
tryRead str =
  case reads str of
    [(res, "")] -> return res
    _ -> fail $ "Cannot read: " ++ str

tryReadInt ∷ (Stream s m Char, Num a) ⇒ String → ParsecT s st m a
tryReadInt str =
  if all isDigit str
    then return $ fromIntegral $ foldl (\a b → 10*a+b) 0 $ map digitToInt str
    else fail $ "Cannot read: " ++ str

-- | Apply parser N times
times ∷ (Stream s m Char)
     ⇒ Int
     → ParsecT s st m t
     → ParsecT s st m [t]
times 0 _ = return []
times n p = do
  ts ← times (n-1) p
  t ← optionMaybe p
  case t of
    Just t' → return (ts ++ [t'])
    Nothing → return ts
                               
-- | Parse natural number of N digits
-- which is not greater than M
number ∷ Stream s m Char
       ⇒ Int   -- ^ Number of digits
       → Int   -- ^ Maximum value
       → ParsecT s st m Int
number n m = do
  t ← tryReadInt =<< (n `times` digit)
  if t > m
    then fail "number too large"
    else return t

pYear ∷ Stream s m Char => ParsecT s st m Int
pYear = do
  y ← number 4 10000
  if y < 2000
    then return (y+2000)
    else return y

pMonth ∷ Stream s m Char => ParsecT s st m Int
pMonth = number 2 12

pDay ∷ Stream s m Char => ParsecT s st m Int
pDay = number 2 31

