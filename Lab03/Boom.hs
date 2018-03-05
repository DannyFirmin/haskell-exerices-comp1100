module Boom where

boom :: Integer -> Integer -> Integer
boom :: Integer -> Integer -> Integer
boom a b
  | a <= 1 || b <= 1 = a
  | otherwise        = a * boom (boom (a - 1) b) (b - 1)
