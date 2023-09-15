{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable, FlexibleContexts #-}
-- | Operations with dates
module Data.Dates
  (DateTime (..),
   Time (..),
   WeekDay (..),
   parseDate, parseDateTime,
   pDate, pDateTime, pTime,
   pDateInterval,
   getCurrentDateTime,
   tryRead, tryReadInt,
   DateIntervalType (..),
   DateInterval (..),
   dayToDateTime, dateTimeToDay,
   weekdayToInterval,
   weekdayNumber,
   intToWeekday,
   dateWeekDay,
   lastMonday, nextMonday,
   modifyDate,
   datesDifference,
   addInterval, negateInterval, minusInterval,
   addTime
  ) where

import Prelude.Unicode
import Data.Char (toUpper)
import Data.List
import Data.Time.Calendar
  ( Day, toGregorian, fromGregorian, addDays, addGregorianMonthsClip
  , addGregorianYearsClip, toModifiedJulianDay )
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.LocalTime
  ( getZonedTime, zonedTimeToLocalTime, localDay, localTimeOfDay
  , todHour, todMin, todSec )
import Text.Parsec
import Data.Generics
import Data.Char (toLower)

import Data.Dates.Types
import Data.Dates.Internal

data DateIntervalType = Day | Week | Month | Year
  deriving (Eq,Show,Read,Data,Typeable)

data DateInterval = Days ℤ
                  | Weeks ℤ
                  | Months ℤ
                  | Years ℤ
  deriving (Eq,Show,Data,Typeable)

data WeekDay =
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Eq, Show, Read, Ord, Enum, Bounded, Data, Typeable)

-- | Weekday as interval from Monday, so that
-- weekdayToInterval Monday == 0 and
-- weekdayToInterval Sunday == 6.
weekdayToInterval ∷ WeekDay → DateInterval
weekdayToInterval wd = Days (fromIntegral $ fromEnum wd)

-- | Number of weekday, with Monday == 1 and Sunday == 7.
weekdayNumber ∷ WeekDay → Int
weekdayNumber wd = fromEnum wd + 1

-- | Reverse for weekdayNumber
intToWeekday ∷ Int → WeekDay
intToWeekday i = toEnum (i - 1)

lastMonday ∷ DateTime → DateTime
lastMonday dt = dt `minusInterval` weekdayToInterval (dateWeekDay dt)

nextMonday ∷ DateTime → DateTime
nextMonday dt = lastMonday dt `addInterval` Weeks 1

-- | Get current date and time.
getCurrentDateTime ∷  IO DateTime
getCurrentDateTime = do
  zt ← getZonedTime
  let lt = zonedTimeToLocalTime zt
      ld = localDay lt
      ltod = localTimeOfDay lt
      (y,m,d) = toGregorian ld
      h = todHour ltod
      mins = todMin ltod
      s = round $ todSec ltod
  return $ DateTime (fromIntegral y) m d h mins s

-- | Get weekday of given date.
dateWeekDay ∷ DateTime → WeekDay
dateWeekDay dt =
  let (_,_,wd) = toWeekDate (dateTimeToDay dt)
  in  intToWeekday wd

uppercase ∷ String → String
uppercase = map toUpper

isPrefixOfI ∷  String → String → Bool
p `isPrefixOfI` s = uppercase p `isPrefixOf` uppercase s

lookupS ∷ String → [(String,a)] → Maybe a
lookupS _ [] = Nothing
lookupS k ((k',v):other) | k `isPrefixOfI` k' = Just v
                         | otherwise          = lookupS k other

monthsN ∷ [(String,Int)]
monthsN = zip months [1..]

lookupMonth ∷ String → Maybe Int
lookupMonth n = lookupS n monthsN

date ∷  Int → Int → Int → DateTime
date y m d = DateTime y m d 0 0 0

addTime ∷  DateTime → Time → DateTime
addTime dt t = dt {
                 hour = tHour t + hour dt,
                 minute = tMinute t + minute dt,
                 second = tSecond t + second dt }

euroNumDate ∷ Stream s m Char => ParsecT s st m DateTime
euroNumDate = do
  d ← pDay
  char '.'
  m ← pMonth
  char '.'
  y ← pYear
  return $ date y m d

americanDate ∷ Stream s m Char => ParsecT s st m DateTime
americanDate = do
  y ← pYear
  char '/'
  m ← pMonth
  char '/'
  date y m <$> pDay

euroNumDate' ∷ Stream s m Char => Int → ParsecT s st m DateTime
euroNumDate' year = do
  d ← pDay
  char '.'
  m ← pMonth
  return $ date year m d

americanDate' ∷ Stream s m Char => Int → ParsecT s st m DateTime
americanDate' year = do
  m ← pMonth
  char '/'
  date year m <$> pDay

strDate ∷ Stream s m Char => ParsecT s st m DateTime
strDate = do
  d ← pDay
  space
  ms ← many1 letter
  case lookupMonth ms of
    Nothing → fail $ "unknown month: "++ms
    Just m  → do
      space
      y ← pYear
      notFollowedBy $ char ':'
      return $ date y m d

strDate' ∷ Stream s m Char => Int → ParsecT s st m DateTime
strDate' year = do
  d ← pDay
  space
  ms ← many1 letter
  case lookupMonth ms of
    Nothing → fail $ "unknown month: "++ms
    Just m  → return $ date year m d

time24 ∷ Stream s m Char => ParsecT s st m Time
time24 = do
  h ← number 2 23
  char ':'
  m ← number 2 59
  x ← optionMaybe $ char ':'
  case x of
    Nothing → return $ Time h m 0
    Just _ → do
      s ← number 2 59
      notFollowedBy letter
      return $ Time h m s

ampm ∷ Stream s m Char => ParsecT s st m Int
ampm = do
  s ← many1 letter
  case map toUpper s of
    "AM" → return 0
    "PM" → return 12
    _ → fail "AM/PM expected"

time12 ∷ Stream s m Char => ParsecT s st m Time
time12 = do
  h ← number 2 12
  char ':'
  m ← number 2 59
  x ← optionMaybe $ char ':'
  s ← case x of
            Nothing → return 0
            Just _  → number 2 59
  optional space
  hd ← ampm
  return $ Time (h+hd) m s

pTime ∷ Stream s m Char => ParsecT s st m Time
pTime = choice $ map try [time12, time24]

pAbsDateTime ∷ Stream s m Char => Int → ParsecT s st m DateTime
pAbsDateTime year = do
  date ← choice $ map (try . ($ year)) [
                              const euroNumDate,
                              const americanDate,
                              const strDate,
                              strDate',
                              euroNumDate',
                              americanDate']
  optional $ char ','
  s ← optionMaybe space
  case s of
    Nothing → return date
    Just _ → do
      t ← pTime
      return $ date `addTime` t

pAbsDate ∷ Stream s m Char => Int → ParsecT s st m DateTime
pAbsDate year =
  choice $ map (try . ($ year)) [
                          const euroNumDate,
                          const americanDate,
                          const strDate,
                          strDate',
                          euroNumDate',
                          americanDate']

-- | Convert date from DateTime to Day
dateTimeToDay ∷  DateTime → Day
dateTimeToDay dt = fromGregorian (fromIntegral $ year dt) (month dt) (day dt)

-- | Convert date from Day to DateTime
dayToDateTime ∷  Day → DateTime
dayToDateTime dt =
  let (y,m,d) = toGregorian dt
  in  date (fromIntegral y) m d

-- | Modify DateTime with pure function on Day
modifyDate ∷  (t → Day → Day) → t → DateTime → DateTime
modifyDate fn x dt =
  let date = dayToDateTime $ fn x $ dateTimeToDay dt
  in  date {hour   = hour   dt,
            minute = minute dt,
            second = second dt}

-- | Add date interval to DateTime
addInterval ∷  DateTime → DateInterval → DateTime
addInterval dt (Days ds) = modifyDate addDays ds dt
addInterval dt (Weeks ws) = modifyDate addDays (ws*7) dt
addInterval dt (Months ms) = modifyDate addGregorianMonthsClip ms dt
addInterval dt (Years ys) = modifyDate addGregorianYearsClip ys dt

-- | Negate DateInterval value: Days 3 → Days (-3).
negateInterval ∷ DateInterval → DateInterval
negateInterval (Days n)   = Days (negate n)
negateInterval (Weeks n)  = Weeks (negate n)
negateInterval (Months n) = Months (negate n)
negateInterval (Years n)  = Years (negate n)

-- | Subtract DateInterval from DateTime.
minusInterval ∷ DateTime → DateInterval → DateTime
minusInterval date int = date `addInterval` negateInterval int

-- | Number of days between two dates
datesDifference ∷ DateTime → DateTime → Integer
datesDifference d1 d2 =
  abs $ toModifiedJulianDay (dateTimeToDay d1) -
        toModifiedJulianDay (dateTimeToDay d2)

maybePlural ∷ Stream s m Char => String → ParsecT s st m String
maybePlural str = do
  r ← string str
  optional $ char 's'
  return (capitalize r)

pDateIntervalType ∷ Stream s m Char => ParsecT s st m DateIntervalType
pDateIntervalType = do
  s ← choice $ map maybePlural ["day", "week", "month", "year"]
  case toLower (head s) of
    'd' → return Day
    'w' → return Week
    'm' → return Month
    'y' → return Year
    _ → fail $ "Unknown date interval type: " ++ s

pDateInterval ∷ Stream s m Char => ParsecT s st m DateInterval
pDateInterval = do
  n ← many1 digit
  spaces
  tp ← pDateIntervalType
  case tp of
    Day →   Days   `fmap` tryReadInt n
    Week →  Weeks  `fmap` tryReadInt n
    Month → Months `fmap` tryReadInt n
    Year →  Years  `fmap` tryReadInt n

pRelDate ∷ Stream s m Char => DateTime → ParsecT s st m DateTime
pRelDate date = do
  offs ← try futureDate
     <|> try passDate
     <|> try today
     <|> try tomorrow
     <|> yesterday
  return $ date `addInterval` offs

lastDate ∷ Stream s m Char => DateTime → ParsecT s st m DateTime
lastDate now = do
    string "last"
    spaces
    try byweek <|> try bymonth <|> byyear
  where
    byweek = do
      wd ← try (string "week" >> return Monday) <|> pWeekDay
      let monday = lastMonday now
          monday' = if wd > dateWeekDay now
                      then monday `minusInterval` Weeks 1
                      else monday
      return $ monday' `addInterval` weekdayToInterval wd

    bymonth = do
      string "month"
      return $ now {day = 1}

    byyear = do
      string "year"
      return $ now {month = 1, day = 1}

nextDate ∷ Stream s m Char => DateTime → ParsecT s st m DateTime
nextDate now = do
    string "next"
    spaces
    try byweek <|> try bymonth <|> byyear
  where
    byweek = do
      wd ← try (string "week" >> return Monday) <|> pWeekDay
      let monday = nextMonday now
          monday' = if wd > dateWeekDay now
                      then monday `minusInterval` Weeks 1
                      else monday
      return $ monday' `addInterval` weekdayToInterval wd

    bymonth = do
      string "month"
      return (now `addInterval` Months 1) {day = 1}

    byyear = do
      string "year"
      return (now `addInterval` Years 1) {month = 1, day = 1}

pWeekDay ∷ Stream s m Char => ParsecT s st m WeekDay
pWeekDay = do
  w ← many1 (oneOf "mondaytueswnhrfi")
  case map toLower w of
    "monday"    → return Monday
    "tuesday"   → return Tuesday
    "wednesday" → return Wednesday
    "thursday"  → return Thursday
    "friday"    → return Friday
    "saturday"  → return Saturday
    "sunday"    → return Sunday
    _           → fail $ "Unknown weekday: " ++ w

futureDate ∷ Stream s m Char => ParsecT s st m DateInterval
futureDate = do
  string "in "
  n ← many1 digit
  char ' '
  tp ← pDateIntervalType
  case tp of
    Day →   Days   `fmap` tryReadInt n
    Week →  Weeks  `fmap` tryReadInt n
    Month → Months `fmap` tryReadInt n
    Year →  Years  `fmap` tryReadInt n

passDate ∷ Stream s m Char => ParsecT s st m DateInterval
passDate = do
  n ← many1 digit
  char ' '
  tp ← pDateIntervalType
  string " ago"
  case tp of
    Day →   (Days   . negate) `fmap` tryReadInt n
    Week →  (Weeks  . negate) `fmap` tryReadInt n
    Month → (Months . negate) `fmap` tryReadInt n
    Year →  (Years  . negate) `fmap` tryReadInt n

today ∷ Stream s m Char => ParsecT s st m DateInterval
today = do
  string "today" <|> string "now"
  return $ Days 0

tomorrow ∷ Stream s m Char => ParsecT s st m DateInterval
tomorrow = do
  string "tomorrow"
  return $ Days 1

yesterday ∷ Stream s m Char => ParsecT s st m DateInterval
yesterday = do
  string "yesterday"
  return $ Days (-1)

pByWeek ∷ Stream s m Char => DateTime → ParsecT s st m DateTime
pByWeek date =
  try (lastDate date) <|> nextDate date

-- | Parsec parser for DateTime.
pDateTime ∷ Stream s m Char => DateTime       -- ^ Current date / time, to use as base for relative dates
          → ParsecT s st m DateTime
pDateTime date =
      try (pRelDate date)
  <|> try (pByWeek date)
  <|> try (pAbsDateTime $ year date)

-- | Parsec parser for Date only.
pDate ∷ Stream s m Char => DateTime       -- ^ Current date / time, to use as base for relative dates
          → ParsecT s st m DateTime
pDate date =
      try (pRelDate date)
  <|> try (pByWeek date)
  <|> try (pAbsDate $ year date)

-- | Parse date
parseDate ∷ DateTime  -- ^ Current date / time, to use as base for relative dates
          → String    -- ^ String to parse
          → Either ParseError DateTime
parseDate date = runParser (pDate date) () ""

-- | Parse date and time
parseDateTime ∷ DateTime  -- ^ Current date / time, to use as base for relative dates
          → String    -- ^ String to parse
          → Either ParseError DateTime
parseDateTime date = runParser (pDateTime date) () ""

