module Days where

data DayNames = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Enum, Show)
dayNameToISODayNo :: DayNames -> Integer
dayNameToISODayNo day = case day of
  Monday    -> 1
  Tuesday   -> 2
  Wednesday -> 3
  Thursday  -> 4
  Friday    -> 5
  Saturday  -> 6
  Sunday    -> 7
