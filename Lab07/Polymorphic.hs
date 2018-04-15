module Polymorphic where

import Data.Complex
import Data.Ratio
import Prelude hiding (product)

stringLength :: String -> Int
stringLength = undefined -- TODO

integerListLength :: [Integer] -> Int
integerListLength = undefined -- TODO

-- | A polymorphic length function
polymorphicLength :: [a] -> Int
polymorphicLength list = case list of
  []     -> 0
  _ : xs -> 1 + polymorphicLength xs

-- | A polymorphic reverse function
reverseOf :: a -- TODO
reverseOf = undefined -- TODO

-- | A polymorphic isPalindrome function
isPalindrome :: a -- TODO
isPalindrome = undefined -- TODO

-- | A polymorphic list equality function
listEqual :: [a] -> [a] -> Bool
listEqual = undefined -- TODO

isMonotonicallyIncreasing :: undefined -- TODO
isMonotonicallyIncreasing = undefined -- TODO

normaliseVector :: undefined -- TODO
normaliseVector = undefined

divideByScalar :: undefined -- TODO
divideByScalar = undefined -- TODO

norm :: undefined -- TODO
norm = undefined -- TODO

reallyPolymorphicLength :: (Integral b) => [a] -> b
reallyPolymorphicLength list = case list of
  []    -> 0
  _: xs -> 1 + reallyPolymorphicLength xs

printLargestAsString :: (Ord a, Show a) => [a] -> String
printLargestAsString list =
  "The largest element in the list is " ++ show (maximum list)

rationalZero :: Ratio Integer
floatZero    :: Float
doubleZero   :: Double
complexZero  :: Complex Double

rationalZero = 0
floatZero    = 0.0
doubleZero   = 0.0
complexZero  = mkPolar 0.0 0.0

product :: undefined -- TODO
product = undefined -- TODO
