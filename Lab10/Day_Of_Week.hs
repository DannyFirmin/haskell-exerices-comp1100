-- File name: Day_Of_Week.hs
-- Author: Danny Feng, (u6611178)
-- Date: 7 May 2018
-- Description: Provides functions to assist in calculating
--              the day of the week.
module Day_Of_Week where

import Dates
import Integer_Subtypes
{-# ANN module ("HLint: ignore Use camelCase"::String) #-}

-- | Compute the number of days since 1 January 0000.
-- >>> days_since_1_January_0 (Date 1 January 0000)
-- 0
-- >>> days_since_1_January_0 (Date 1 January 0001)
-- 366
-- >>> days_since_1_January_0 (Date 1 January 0004)
-- 1461
-- >>> days_since_1_January_0 (Date 1 March 0004)
-- 1521
-- >>> days_since_1_January_0 (Date 1 January 0005)
-- 1827
-- >>> days_since_1_January_0 (Date 1 March 0005)
-- 1886
-- >>> days_since_1_January_0 (Date 9 October 1993)
-- 728210
days_since_1_January_0 :: Date -> Natural
days_since_1_January_0 (Date day month year) =
  previous_days_this_month day +
  days_in_previous_months month year +
  days_before_this_year year

previous_days_this_month :: Day -> Natural
previous_days_this_month d = from_Positive_to_Natural d-1

days_in_previous_months :: Month -> Year -> Natural
days_in_previous_months month year =
  sum (take (fromEnum month) (month_lengths year))
  where
    month_lengths :: Year -> [Natural]
    month_lengths year = [31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    feb
      | is_leap_year year = 29
      | otherwise = 28

days_in_previous_months' :: Month -> Year -> Natural
days_in_previous_months' month year
  | is_leap_year year = case month of
        January   -> 0
        February  -> 31
        March     -> 60
        April     -> 91
        May       -> 121
        June      -> 152
        July      -> 182
        August    -> 213
        September -> 244
        October   -> 274
        November  -> 305
        December  -> 335
  | otherwise = case month of
        January   -> 0
        February  -> 31
        March     ->  59
        April     ->  90
        May       -> 120
        June      -> 151
        July      -> 181
        August    -> 212
        September -> 243
        October   -> 273
        November  -> 304
        December  -> 334

is_leap_year :: Year -> Bool
is_leap_year y
 | y `mod` 4 == 0 && y `mod` 100 /= 0  = True
 | y `mod` 4 == 0 && y `mod` 400 == 0 = True
 | otherwise = False

leap_years_since_1_January_0 :: Year -> Natural
leap_years_since_1_January_0 year = case year of
    0 -> 0
    _ -> 1 + pre_year `quot` 4 - pre_year `quot` 100 + pre_year `quot` 400
        where pre_year = year - 1


days_before_this_year :: Year -> Natural
days_before_this_year y
  |y == 1 = 365
  |is_leap_year y     = 366 + days_before_this_year(y-1)
  |not (is_leap_year y) = 365 + days_before_this_year(y-1)


day_of_week :: Date -> Days
day_of_week n
  |toInteger(days_since_1_January_0 n) `mod` 7 == 0 = Saturday
  |toInteger(days_since_1_January_0 n) `mod` 7 == 1 = Sunday
  |toInteger(days_since_1_January_0 n) `mod` 7 == 2 = Monday
  |toInteger(days_since_1_January_0 n) `mod` 7 == 3 = Tuesday
  |toInteger(days_since_1_January_0 n) `mod` 7 == 4 = Wednesday
  |toInteger(days_since_1_January_0 n) `mod` 7 == 5 = Thursday
  |toInteger(days_since_1_January_0 n) `mod` 7 == 6 = Friday

