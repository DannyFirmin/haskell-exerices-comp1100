-- File name: Day_Of_Week.hs
-- Author: <your name>, u<your uni id>
-- Date: <today's date>
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

previous_days_this_month = undefined --TODO

days_in_previous_months :: Month -> Year -> Natural
days_in_previous_months month year =
  sum (take (fromEnum month) (month_lengths year))
  where
    month_lengths :: Year -> [Natural]
    month_lengths year = [31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    feb
      | is_leap_year year = 29
      | otherwise = 28

is_leap_year = undefined --TODO

days_before_this_year = undefined --TODO

day_of_week :: Date -> Days
day_of_week = undefined --TODO

