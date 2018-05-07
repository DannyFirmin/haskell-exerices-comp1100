--
-- Uwe R. Zimmer, based on 2012 lab code by James Barker:
-- Copyright 2012, The Australian National University
--
-- Description: Provides a barebones implementation
--              of Zeller's congruence, as well as
--              a test generator for dates.
module Zellers_Congruence
  ( zellers_day_of_week -- :: Date -> Days
  ) where

import Dates
import Integer_Subtypes
{-# ANN module ("HLint: ignore Use camelCase"::String) #-}

-- Calculates Zeller's congruence for a given date,
-- according to Wikipedia. (http://en.wikipedia.org/wiki/Zeller's_congruence)
-- The keen reader will notice that the original description includes
-- floor functions -- why are these not needed here?
zellers_congruence :: Date -> Integer
zellers_congruence (Date day month year) =
  (q + ((13 * (m + 1)) `div` 5) + k + (k `div` 4) + (j `div` 4) - 2 * j) `mod` 7
  where
    q = toInteger day
    m =
      case month of
        January -> 13
        February -> 14
        March -> 3
        April -> 4
        May -> 5
        June -> 6
        July -> 7
        August -> 8
        September -> 9
        October -> 10
        November -> 11
        December -> 12
    k
      | month == January || month == February = (toInteger year - 1) `mod` 100
      | otherwise = toInteger year `mod` 100
    j
      | month == January || month == February = (toInteger year - 1) `div` 100
      | otherwise = toInteger year `div` 100

-- Zeller's congruence as given on Wikipedia numbers dates starting from zero for Saturday. 
-- ISO standard day numbers start from one for Monday.
-- This function biases the result to agree with the standard.
adjusted_zellers :: Date -> Positive
adjusted_zellers date =
  fromInteger (1 + (zellers_congruence date - 2) `mod` 7)

-- Calculates the day of the week of a particular date, using Zeller's
-- congruence.
zellers_day_of_week :: Date -> Days
zellers_day_of_week date = iso_day_no_to_name (adjusted_zellers date)
