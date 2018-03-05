module Grades where
  data Grades = HD | D | C | P | N deriving (Show)
  grade :: Integer -> Maybe Grades
  grade x
    | x >= 80 && x <= 100 = Just HD
    | x >= 70 && x <   80 = Just D
    | x >= 60 && x <   70 = Just C
    | x >= 50 && x <   60 = Just P
    | x >=  0 && x <   50 = Just N
    | x <   0 || x >  100 = Nothing
    | otherwise = error "Non-exhaustive guards in function: grade"