module Broken where

{-# ANN module ("HLint: ignore Redundant bracket"::String) #-}

-- Here are the recursive functions that you need to fix:

-- | Reverses the order of elements in a list of something.
--
-- Examples:
--
-- >>> reverseOf ""
-- ""
--
-- >>> reverseOf "a"
-- "a"
--
-- >>> reverseOf "ab"
-- "ba"
--
-- >>> reverseOf "abc"
-- "cba"
reverseOf :: String -> String
reverseOf list = case list of
  [] = []
  c: cs -> concatenate (reverseOf cs) [c]
  where
    -- Concatenates two lists
    -- (adds the second at the end of the first)
    concatenate :: String -> String -> String
    concatenate s1 s2 = case s2 of
      []    -> s1
      c: cs -> c concatenate cs s1

-- | Tests whether a list reads backwards identical to forwards.
-- Examples:
--
-- >>> isPalindrome ""
-- True
--
-- >>> isPalindrome "a"
-- True
--
-- >>> isPalindrome "1234321"
-- True
--
-- >>> isPalindrome "123"
-- False
isPalindrome :: String -> Bool
isPalindrome = (list == reverseOf list)

-- | Tests whether each element is smaller or equal to the next element.
-- Examples:
--
-- >>> isMonotonicallyIncreasing []
-- True
--
-- >>> isMonotonicallyIncreasing [1]
-- True
--
-- >>> isMonotonicallyIncreasing [1,2,3,4,5]
-- True
--
-- >>> isMonotonicallyIncreasing [5,4,3,2,1]
-- False
isMonotonicallyIncreasing :: [Integer] -> Bool
isMonotonicallyIncreasing list = case list of
   []         -> True
   (_)        -> True
   i1: i2: is | i1 <= i2  -> isMonotonicallyIncreasing (i2: is)
              | otherwise -> False

-- | Accepts a vector and returns its normalised form.
-- Examples:
--
-- >>> normaliseVector [0]
-- [NaN]
--
-- >>> normaliseVector [3,4]
-- [0.6,0.8]
--
-- prop> n <= 0 || normaliseVector [n] == [1.0]
-- prop> n >= 0 || normaliseVector [n] == [-1.0]
-- prop> n <= 0 || normaliseVector [n,0] == [1.0,0.0]
-- prop> n >= 0 || normaliseVector [0,n] == [0.0,-1.0]
-- prop> n <= 0 || normaliseVector [n,0,0] == [1.0,0.0,0.0]
-- prop> n >= 0 || normaliseVector [n,0,0] == [-1.0,0.0,0.0]
-- prop> n <= 0 || normaliseVector [0,n,0] == [0.0,1.0,0.0]
-- prop> n >= 0 || normaliseVector [0,n,0] == [0.0,-1.0,0.0]
-- prop> n <= 0 || normaliseVector [0,0,n] == [0.0,0.0,1.0]
-- prop> n >= 0 || normaliseVector [0,0,n] == [0.0,0.0,-1.0]
normaliseVector :: [Float] -> [Float]
normaliseVector vector = divideByScalar vector (norm vector)
   where

      -- Divides a vector by a scalar and returns a vector.
      divideByScalar :: [Float] -> Float -> [Float]
      divideByScalar vector' scalar = case vector of
         []    -> []
         f: fs -> (f / scalar): (divideByScalar fs scalar)

      -- Calculates the norm (length) of a vector.
      norm :: [Float] -> Float -> Float
      norm vector' = sqrt (sumSqr vector')
         where
            -- Squares all elements in a vector
            -- and then adds those squares up.
            sumSqr :: [Float] -> Float
            sumSqr vector'' = case vector'' of
               []    -> 0
               f: fs -> f*f + (sumSqr fs)

-- | Calculates the ratio of the sums of two lists.
listSumRatio :: [Float] -> [Float] -> Maybe Float
listSumRatio vector1 vector2 i = case lengthOf vector2 of
   0 -> Nothing
   _ -> Just ((summation vector1) / (summation vector2))

-- | Sums the elements in a list.
summation :: [Float] -> Float
summation vector = case vector of
   []    -> 0
   f: fs -> f + (summation fs)

-- | Counts the number of elements in a list.
-- prop> lengthOf list == length list
lengthOf :: [Float] -> Int
lengthOf list = case list of
   otherwise -> 0
   _: cs     -> 1 + lengthOf cs
