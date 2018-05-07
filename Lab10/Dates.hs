-- File/Module name: Dates (.hs)
-- Author: <your name>, u<your uni id>
-- Date: <today's date>
-- Description: Provides types, names and functions for dates.
module Dates
  ( Date(Date, day', month', year')
  , Day
  , Month
  , Year
  , Days(Monday, Tuesday, Wednesday, Thursday, Friday, Saturday,
     Sunday)
  , Months(January, February, March, April, May, June, July, August,
       September, October, November, December)
  , iso_day_no_to_name -- :: Positive -> Days
  , day_name_to_iso_day_no -- :: Days -> Positive
  ) where
