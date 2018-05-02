module HigherOrder where

import Prelude hiding (product, sum)
-- import Data.Char
{-# ANN module ("HLint: ignore Unnecessary hiding"::String) #-}
{-# ANN module ("HLint: ignore Use sum"::String) #-}
{-# ANN module ("HLint: ignore Use product"::String) #-}
{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}

-- | isEqual
-- Examples:
--
-- >>> isEqual 5 5.9999
-- False
--
-- >>> isEqual 6 5.9999
-- True
isEqual :: Integer -> Double -> Bool
isEqual m x = abs (fromIntegral m - x) < tolerance
  where
    tolerance = 0.0001

-- | flipArguments
-- Examples:
--
-- >>> flipArguments isEqual 5.9999 6
-- True
flipArguments :: (Integer -> Double -> Bool) -> Double -> Integer -> Bool
flipArguments func doubleValue integerValue = func integerValue doubleValue

-- | applyFunction
applyFunction :: (Integer -> Integer) -> Integer -> Integer
applyFunction func m = func m

double :: Integer -> Integer
double m = 2 * m

triple :: Integer -> Integer
triple m = m + m + m

reverseSign :: Integer -> Integer
reverseSign m = -m

-- | applyFunctionOverList
-- Examples:
--
-- >>> applyFunctionOverList double [1,2,3]
-- [2,4,6]
--
-- >>> applyFunctionOverList reverseSign [1,2,3]
-- [-1,-2,-3]
applyFunctionOverList :: (Integer -> Integer) -> [Integer] -> [Integer]
applyFunctionOverList _ [] = []
applyFunctionOverList func (x:xs) = [func x]++ applyFunctionOverList func xs

-- | selectWhereTrue
-- Examples:
--
--  >>> selectWhereTrue isNegative [0.0, 1.0, -1.0, -9.2, 3.0]
-- [-1.0,-9.2]
--
-- >>> selectWhereTrue isPositive [0.0, 1.0, -1.0, -9.2, 3.0]
-- [1.0,3.0]
selectWhereTrue :: (Double -> Bool) -> [Double] -> [Double]
selectWhereTrue _ [] = []
selectWhereTrue func (x:xs)
  |func x == True = [x] ++ selectWhereTrue func xs
  |otherwise = selectWhereTrue func xs

isNegative :: Double -> Bool
isNegative x = x < 0.0

isPositive :: Double -> Bool
isPositive x = x > 0.0

-- | polymorphicLength
polymorphicLength :: Integral b => [a] -> b
polymorphicLength list =
  case list of
    [] -> 0
    _:xs -> 1 + polymorphicLength xs

-- | applyFunction'
applyFunction' :: (a -> Integer) -> a -> Integer
applyFunction' f m = f m

-- | applyFunction''
applyFunction'' :: (a -> b) -> a -> b
applyFunction'' f x = f x

-- | applyFunctionOverList'
--       applyFunctionOverList :: (Integer -> Integer) -> [Integer] -> [Integer]
applyFunctionOverList' :: (Num a)=> (a -> b) -> [a] -> [b]
applyFunctionOverList' _ [] = []
applyFunctionOverList' func (x:xs) = [func x]++ applyFunctionOverList' func xs

-- | selectWhereTrue'
--       selectWhereTrue :: (Double -> Bool) -> [Double] -> [Double]
selectWhereTrue' :: (Num a)=> (a -> Bool) -> [a] -> [a]
selectWhereTrue' _ [] = []
selectWhereTrue' func (x:xs)
  |func x == True = [x] ++ selectWhereTrue' func xs
  |otherwise = selectWhereTrue' func xs
-- | combineListsWithBinaryOperation
-- Examples:
--
-- >>> combineListsWithBinaryOperation (+) [1.0, 2.0, 3.0] [4.0, 5.0, 6.0]
-- [5.0, 7.0, 9.0]
--
-- >>> combineListsWithBinaryOperation (++) ["the", "brown", "jumps", "the"] ["quick", "fox", "over", "lazy", "dog"]
-- ["thequick", "brownfox", "jumpsover", "thelazy"]
--
-- >>> combineListsWithBinaryOperation div [1..10] [-10..0]
-- [-1, -1, -1, -1, -1, -2, -2, -3, -5, -10]
combineListsWithBinaryOperation :: (a->b->c)-> [a] -> [b] -> [c]
combineListsWithBinaryOperation f (x:xs) (y:ys) = f x y:combineListsWithBinaryOperation f xs ys
combineListsWithBinaryOperation _ _ _ = []

-- | combineElementsIntoTuples
combineElementsIntoTuples :: [a] -> [b] -> [(a, b)]
combineElementsIntoTuples (x:xs) (y:ys) = (x,y) : combineElementsIntoTuples xs ys
combineElementsIntoTuples _ _ = []

-- | combineElementsIntoTuples'

--combineElementsIntoTuples' :: ((a->b->c)-> [a] -> [b] -> [c]) ->[(a, b)]
--combineElementsIntoTuples' f a b= combineListsWithBinaryOperation a b


-- | foldRight
-- Examples:
--
-- >>> foldRight (+) 0 [1,2,3,4,5]
-- 15
--
-- >>> foldRight (*) 1 [1,2,3,4,5]
-- 120
--
-- >>> foldRight (-) 0 [1,2,3,4,5]
-- 3
foldRight :: (a -> b -> b) -> b -> [a] -> b -- predefined as foldr
foldRight _ e [] = e
foldRight f e (x:xs) = f x (foldRight f e xs)
-- foldRight f e (x:xs) = foldRight f (f x e) xs

-- | foldLeft
-- Examples:
--
-- >>> foldLeft (+) 0 [1,2,3,4,5]
-- 15
--
-- >>> foldLeft (-) 0 [1,2,3,4,5]
-- -15
--
-- prop> foldLeft (+) 0 xs == foldRight (+) 0 xs
--
-- The following property does not hold:
-- prop> foldLeft (-) 0 xs == foldRight (-) 0 xs
foldLeft :: (b -> a -> b) -> b -> [a] -> b -- predefined as foldl
foldLeft _ e [] = e
foldLeft f e (x:xs) = foldLeft f (f e x) xs

-- | Reimplement sum using foldLeft or foldRight
sum :: Num a => [a] -> a
sum list =
  case list of
   (x:xs) -> foldRight (+) x xs
   [] -> 0

-- | Reimplement product using foldLeft or foldRight
product :: Num a => [a] -> a
product list =
  case list of
    (x:xs) -> foldRight (*) x xs
    [] -> 0

-- | Reimplement allTrue using foldLeft or foldRight
allTrue :: [Bool] -> Bool
allTrue list =
  case list of
    (x:xs) -> foldRight (&&) x xs
    [] -> False


-- | Reimplement anyTrue using foldLeft or foldRight
anyTrue :: [Bool] -> Bool
anyTrue list =
  case list of
    (x:xs) -> foldRight (||) x xs
    [] -> False

-- -- | convertToLower
-- convertToLower ::[Char] -> [Char]
-- convertToLower a = map toLower a
--
-- -- | removeNonAlphanum
-- removeNonAlphanum :: [Char] -> [Char]
-- removeNonAlphanum a = filter isAlphaNum a
--
-- -- | dotProduct
-- dotProduct :: (Num a) => [a] -> [a] -> a
-- dotProduct l1 l2 = sum (map (\(a,b) -> a*b) (zip l1 l2)

-- | isSquare
-- Examples:
--
-- >>> isSquare (10000 :: Int)
-- True
--
-- >>> isSquare (10000 :: Integer)
-- True
isSquare :: Integral a => a -> Bool
isSquare i = floor (sqrt (fromIntegral i) :: Float) ^ (2 :: Int) == i

-- | sumOfSquaresUpTo
-- Example:
--
-- >>> sumOfSquaresUpTo 1000
-- 10416
sumOfSquaresUpTo :: Int -> Int
sumOfSquaresUpTo n = sum (filter isSquare [0..n])

-- | largestSquareSmallerThan
-- Example:
--
-- >>> largestSquareSmallerThan 1000
-- 961
largestSquareSmallerThan :: Int -> Int
largestSquareSmallerThan n = last (filter isSquare [0..n])

-- | isPrime
-- Examples:
--
-- >>> isPrime 2
-- True
--
-- >>> isPrime 3
-- True
--
-- >>> isPrime 4
-- False
--
-- >>> isPrime 5
-- True
--
-- >>> isPrime 6
-- False
--
-- >>> isPrime 7
-- True
isPrime :: Integer -> Bool
isPrime = undefined -- TODO

-- | primeFactors
primeFactors :: Integer -> [Integer]
primeFactors = undefined -- TODO
