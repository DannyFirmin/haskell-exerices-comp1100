module Grades where

grade :: Integer -> String
grade x
  | x >= 80 && x <= 100 = "High Distinction"
  | x >= 70 && x <   80 = "Distinction"
  | x >= 60 && x <   70 = "Credit"
  | x >= 50 && x <   60 = "Pass"
  | x >=  0 && x <   50 = "Fail"
