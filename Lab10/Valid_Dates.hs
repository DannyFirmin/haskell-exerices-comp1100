--
-- Uwe R. Zimmer, based on 2012 lab code by James Barker
-- Copyright 2013 The Australian National University
--
module Valid_Dates
  ( valid_dates -- :: Gen Date
  ) where

import Dates
import Integer_Subtypes
import System.Random
import Test.QuickCheck
{-# ANN module ("HLint: ignore Use camelCase"::String) #-}

--------------------------------------------------
-- Generator to create random dates for testing --
--------------------------------------------------
-- This may be confusing to read and you are not expected to understand it in first year.
-- Many concepts used here have not yet been introduced in your course (or never will be).
-- The only reason why this code is here is to provide you with a random generator for dates (valid_dates).
-- Its usage is explained in the accompanying lab text.
enumRandom :: (RandomGen g, Enum e) => g -> (e, g)
enumRandom gen =
  let (int, gen') = random gen
   in (toEnum int, gen')

enumRandomR :: (RandomGen g, Enum e) => (e, e) -> g -> (e, g)
enumRandomR (lo, hi) gen =
  let (int, gen') = randomR (fromEnum lo, fromEnum hi) gen
   in (toEnum int, gen')

instance Random Months where
  random = enumRandom
  randomR = enumRandomR

is_leap_year :: Natural -> Bool
is_leap_year year
  | year `mod` 4 == 0 && year `mod` 100 /= 0 = True
  | year `mod` 4 == 0 && year `mod` 100 == 0 && year `mod` 400 == 0 = True
  | otherwise = False

valid_dates :: Gen Date
valid_dates = do
  year <- choose (1582 :: Natural, 2100 :: Natural)
  month <- choose (January, December)
  day <- day_generator month year
  return (Date day month year)
  where
    months_with_thirty_days = [April, June, September, November]
    months_with_thirty_one_days =
      [January, March, May, July, August, October, December]
    day_generator month year
      | month `elem` months_with_thirty_days = choose (1, 30)
      | month `elem` months_with_thirty_one_days = choose (1, 31)
      | is_leap_year year = choose (1, 29)
      | otherwise = choose (1, 28)
