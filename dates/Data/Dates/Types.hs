{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Data.Dates.Types
  (DateTime (..),
   Time (..),
   months, capitalize
  ) where

import Prelude.Unicode
import Data.Semigroup
import Data.Monoid
import Data.Char
import Data.Generics

-- | Date / Time
data DateTime =
  DateTime {
    year   ∷ Int,
    month  ∷ Int,
    day    ∷ Int,
    hour   ∷ Int,
    minute ∷ Int,
    second ∷ Int }
  deriving (Eq,Ord,Data,Typeable)

-- | 12 months names.
months ∷ [String]
months = ["january",
          "february",
          "march",
          "april",
          "may",
          "june",
          "july",
          "august",
          "september",
          "october",
          "november",
          "december"]

-- | capitalize first letter of the string
capitalize ∷ String → String
capitalize [] = []
capitalize (x:xs) = (toUpper x):xs

-- | Show name of given month
showMonth ∷  Int → String
showMonth i = capitalize $ months !! (i-1)

instance Show DateTime where
  show (DateTime y m d h mins s) = 
    show d ⧺ " " ⧺ showMonth m ⧺ " " ⧺ show y ⧺ ", " ⧺
      show h ⧺ ":" ⧺ show mins ⧺ ":" ⧺ show s

-- | Only time, without date
data Time = 
  Time {
    tHour   ∷ Int,
    tMinute ∷ Int,
    tSecond ∷ Int }
  deriving (Eq,Ord,Show,Data,Typeable)

instance Semigroup DateTime where
  dt1 <> dt2 = 
      DateTime (year dt1   `plus` year dt2)
               (month dt1  `plus` month dt2)
               (day dt1    `plus` day dt2)
               (hour dt1   `plus` hour dt2)
               (minute dt1 `plus` minute dt2)
               (second dt1 `plus` second dt2)
    where
      plus :: Int → Int → Int
      plus 0 x = x
      plus x _ = x

instance Monoid DateTime where
  mempty = DateTime 0 0 0 0 0 0
  mappend = (Data.Semigroup.<>)

