--
-- Uwe R. Zimmer
-- Copyright 2014 The Australian National University
--
module Integer_Subtypes
  ( Natural -- Integer subtype range 0 .. ; providing (Num, Real, Enum, Integral, Show, Read, Random) deriving (Eq, Ord)
  , Positive -- Integer subtype range 1 .. ; providing (Num, Real, Enum, Integral, Show, Read, Random) deriving (Eq, Ord)
  , Int_Range -- Int subtype range minBound .. maxBound; providing (Bounded, Num, Real, Enum, Integral, Show, Read, Random, Binary) deriving (Eq, Ord)
  , Nat -- Int subtype range 0        .. maxBound; providing (Bounded, Num, Real, Enum, Integral, Show, Read, Random, Binary) deriving (Eq, Ord)
  , Pos -- Int subtype range 1        .. maxBound; providing (Bounded, Num, Real, Enum, Integral, Show, Read, Random, Binary) deriving (Eq, Ord)
   -- Conversions from and to Integer are implicit, yet 
   -- conversions between Natural and Positive directly are explicit:
  , from_Natural_to_Positive -- :: Natural -> Positive
  , from_Positive_to_Natural -- :: Positive -> Natural
   -- All conversions between range limited types are explicit:
  , from_Int_to_Int_Range -- :: Int -> Int_Range
  , from_Int_to_Nat -- :: Int -> Nat
  , from_Int_to_Pos -- :: Int -> Pos
  , from_Int_Range_to_Int -- :: Int_Range -> Int
  , from_Int_Range_to_Nat -- :: Int_Range -> Nat
  , from_Int_Range_to_Pos -- :: Int_Range -> Pos
  , from_Nat_to_Int_Range -- :: Nat -> Int_Range
  , from_Nat_to_Int -- :: Nat -> Int
  , from_Nat_to_Pos -- :: Nat -> Pos
  , from_Pos_to_Int_Range -- :: Pos -> Int_Range
  , from_Pos_to_Int -- :: Pos -> Int
  , from_Pos_to_Nat -- :: Pos -> Nat
  ) where

import Control.Monad (liftM)
import Data.Binary (Binary, Get, get, put)
import Data.Int (Int64)
import System.Random (Random, random, randomR)
{-# ANN module ("HLint: ignore Use camelCase"::String) #-}

-- Motivation for this module:
--
-- Haskell does not provide the concept of subtypes (which would allow to 
-- restrict a type while keeping it compatible with a base type). Yet it relies 
-- heavily on recursion which is defined over the natural or positive
-- numbers in many cases. Thus a type Natural or Positive is the 
-- canonical type choice for those functions (the length of a list for 
-- instance cannot return a negative number).
--
-- The language defined, implicit conversions to and from Integer provide 
-- most of the functionality for subtypes of Integer (while direct conversions
-- between the subtypes still need to be done explicitely). In the case of
-- subtypes for Int, all required conversions need to be done explicitely.
--
-- It is recommended to use Natural and Positive (or the restricted range
-- equivalents Nat and Pos) as much as possible, in order to ease the effort 
-- of prooving correctness and to write meaningful tests for functions which 
-- were never meant to handle negative numbers. 
--
-- Range violations are detected and treated as an error 
-- (no "wrap around" arithmetic) on all limited precision types.
--
-- Note that numerical literals always convert implicitely into any of these
-- types and the type constructors (which would be unsafe to use) are not
-- exported from this module.
--
newtype Natural =
  Natural Integer
  deriving (Eq, Ord)

newtype Positive =
  Positive Integer
  deriving (Eq, Ord)

newtype Int_Range =
  Int_Range Int
  deriving (Eq, Ord)

newtype Nat =
  Nat Int
  deriving (Eq, Ord)

newtype Pos =
  Pos Int
  deriving (Eq, Ord)

-- Conversions between Natural and Positive:
from_Natural_to_Positive :: Natural -> Positive
from_Natural_to_Positive (Natural x)
  | check_Positive x = Positive x
  | otherwise = error (out_1 "from_Natural_to_Positive" x "Positive")

from_Positive_to_Natural :: Positive -> Natural
from_Positive_to_Natural (Positive x) = Natural x

-- Conversions from Int:
from_Int_to_Int_Range :: Int -> Int_Range
from_Int_to_Int_Range = Int_Range

from_Int_to_Nat :: Int -> Nat
from_Int_to_Nat x
  | check_Nat x = Nat x
  | otherwise = error (out_1 "from_Int_to_Nat" x "Nat")

from_Int_to_Pos :: Int -> Pos
from_Int_to_Pos x
  | check_Pos x = Pos x
  | otherwise = error (out_1 "from_Int_to_Pos" x "Pos")

-- Conversions from Int_Range:
from_Int_Range_to_Int :: Int_Range -> Int
from_Int_Range_to_Int (Int_Range x) = x

from_Int_Range_to_Nat :: Int_Range -> Nat
from_Int_Range_to_Nat (Int_Range x)
  | check_Nat x = Nat x
  | otherwise = error (out_1 "from_Int_Range_to_Nat" x "Nat")

from_Int_Range_to_Pos :: Int_Range -> Pos
from_Int_Range_to_Pos (Int_Range x)
  | check_Pos x = Pos x
  | otherwise = error (out_1 "from_Int_Range_to_Pos" x "Pos")

-- Conversions from Nat:
from_Nat_to_Int_Range :: Nat -> Int_Range
from_Nat_to_Int_Range (Nat x) = Int_Range x

from_Nat_to_Int :: Nat -> Int
from_Nat_to_Int (Nat x) = x

from_Nat_to_Pos :: Nat -> Pos
from_Nat_to_Pos (Nat x)
  | check_Pos x = Pos x
  | otherwise = error (out_1 "from_Nat_to_Pos" x "Pos")

-- Conversions from Pos:
from_Pos_to_Int_Range :: Pos -> Int_Range
from_Pos_to_Int_Range (Pos x) = Int_Range x

from_Pos_to_Int :: Pos -> Int
from_Pos_to_Int (Pos x) = x

from_Pos_to_Nat :: Pos -> Nat
from_Pos_to_Nat (Pos x) = Nat x

-- Lower range checks for all types:
-- mind that checks for Int bounds need to be done before the
-- Int value is calculated and thus those checks will appear 
-- inside individual arithmetic operations.
check_Natural :: Integer -> Bool
check_Natural x = x >= 0

check_Positive :: Integer -> Bool
check_Positive x = x > 0

check_Nat :: Int -> Bool
check_Nat x = x >= 0

check_Pos :: Int -> Bool
check_Pos x = x > 0

-- Instances for Natural
instance Num Natural where
  (Natural x) + (Natural y) = Natural (x + y)
  (Natural x) - (Natural y)
    | check_Natural diff = Natural diff
    | otherwise = error (out_2 x "-" y "Natural")
    where
      diff = x - y
  (Natural x) * (Natural y) = Natural (x * y)
  negate (Natural x)
    | x == 0 = Natural x
    | otherwise = error (out_1 "negate" x "Natural")
  abs (Natural x) = Natural x
  signum (Natural x)
    | check_Natural sign_x = Natural sign_x
    | otherwise = error (out_1 "signum" x "Natural")
    where
      sign_x = signum x
  fromInteger x
    | check_Natural x = Natural (fromInteger x)
    | otherwise = error (out_1 "fromInteger" x "Natural")

instance Real Natural where
  toRational (Natural x)
    | check_Natural x = toRational x
    | otherwise = error (out_1 "toRational" x "Natural")

instance Enum Natural where
  succ (Natural x) = Natural (succ x)
  pred (Natural x)
    | check_Natural pred_x = Natural pred_x
    | otherwise = error (out_1 "pred" x "Natural")
    where
      pred_x = pred x
  toEnum x
    | check_Natural toInteger_x = Natural toInteger_x
    | otherwise = error (out_1 "toEnum" x "Natural")
    where
      toInteger_x = toInteger x
  fromEnum (Natural x)
    | check_Natural x = fromInteger x
    | otherwise = error (out_1 "fromEnum" x "Natural")

instance Integral Natural where
  (Natural x) `quot` (Natural y) = Natural (x `quot` y)
  (Natural x) `rem` (Natural y) = Natural (x `rem` y)
  (Natural x) `div` (Natural y) = Natural (x `div` y)
  (Natural x) `mod` (Natural y) = Natural (x `mod` y)
  x `quotRem` y = (x `quot` y, x `rem` y)
  x `divMod` y = (x `div` y, x `mod` y)
  toInteger (Natural x) = x

instance Show Natural where
  show (Natural x) = show x

instance Read Natural where
  readsPrec i str = [(result, rest)]
    where
      [(result, rest)] = readsPrec i str

instance Random Natural where
  random gen = (Natural x, gen')
    where
      (x, gen') = randomR (0, toInteger (maxBound :: Int)) gen
  randomR (Natural from, Natural to) gen = (Natural x, gen')
    where
      (x, gen') = randomR (from, to) gen

-- Instances for Positive
instance Num Positive where
  (Positive x) + (Positive y) = Positive (x + y)
  (Positive x) - (Positive y)
    | check_Positive diff = Positive diff
    | otherwise = error (out_2 x "-" y "Positive")
    where
      diff = x - y
  (Positive x) * (Positive y) = Positive (x * y)
  negate (Positive x) = error (out_1 "negate" x "Positive")
  abs (Positive x) = Positive x
  signum (Positive x)
    | check_Positive sign_x = Positive sign_x
    | otherwise = error (out_1 "signum" x "Positive")
    where
      sign_x = signum x
  fromInteger x
    | check_Positive x = Positive (fromInteger x)
    | otherwise = error (out_1 "fromInteger" x "Positive")

instance Real Positive where
  toRational (Positive x)
    | check_Positive x = toRational x
    | otherwise = error (out_1 "toRational" x "Positive")

instance Enum Positive where
  succ (Positive x) = Positive (succ x)
  pred (Positive x)
    | check_Positive pred_x = Positive pred_x
    | otherwise = error (out_1 "pred" x "Positive")
    where
      pred_x = pred x
  toEnum x
    | check_Positive toInteger_x = Positive toInteger_x
    | otherwise = error (out_1 "toEnum" x "Positive")
    where
      toInteger_x = toInteger x
  fromEnum (Positive x)
    | check_Positive x = fromInteger x
    | otherwise = error (out_1 "fromEnum" x "Positive")

instance Integral Positive where
  (Positive x) `quot` (Positive y)
    | check_Positive x_quot_y = Positive x_quot_y
    | otherwise = error (out_2 x "quot" y "Positive")
    where
      x_quot_y = x `quot` y
  (Positive x) `rem` (Positive y)
    | check_Positive x_rem_y = Positive x_rem_y
    | otherwise = error (out_2 x "rem" y "Positive")
    where
      x_rem_y = x `rem` y
  (Positive x) `div` (Positive y)
    | check_Positive x_div_y = Positive x_div_y
    | otherwise = error (out_2 x "div" y "Positive")
    where
      x_div_y = x `div` y
  (Positive x) `mod` (Positive y)
    | check_Positive x_mod_y = Positive x_mod_y
    | otherwise = error (out_2 x "mod" y "Positive")
    where
      x_mod_y = x `mod` y
  x `quotRem` y = (x `quot` y, x `rem` y)
  x `divMod` y = (x `div` y, x `mod` y)
  toInteger (Positive x) = x

instance Show Positive where
  show (Positive x) = show x

instance Read Positive where
  readsPrec i str = [(result, rest)]
    where
      [(result, rest)] = readsPrec i str

instance Random Positive where
  random gen = (Positive x, gen')
    where
      (x, gen') = randomR (1, toInteger (maxBound :: Int)) gen
  randomR (Positive from, Positive to) gen = (Positive x, gen')
    where
      (x, gen') = randomR (from, to) gen

-- Instances for Int_Range
instance Bounded Int_Range where
  minBound = Int_Range minBound
  maxBound = Int_Range maxBound

instance Num Int_Range where
  (Int_Range x) + (Int_Range y)
    | y <= maxBound - x = Int_Range i_sum
    | otherwise = error (out_2 x "+" y "Int_Range")
    where
      i_sum = x + y
  (Int_Range x) - (Int_Range y)
    | y <= minBound + x = Int_Range diff
    | otherwise = error (out_2 x "-" y "Int_Range")
    where
      diff = x - y
  (Int_Range x) * (Int_Range y)
    | prod <= toInteger (maxBound :: Int) && prod >= toInteger (minBound :: Int) =
      Int_Range (fromInteger prod)
    | otherwise = error (out_2 x "*" y "Int_Range")
    where
      prod = toInteger x * toInteger y
  negate (Int_Range x) = Int_Range (-x)
  abs (Int_Range x) = Int_Range (abs x)
  signum (Int_Range x) = Int_Range (signum x)
  fromInteger x
    | check_Natural x &&
        x <= toInteger (maxBound :: Int) && x >= toInteger (minBound :: Int) =
      Int_Range (fromInteger x)
    | otherwise = error (out_1 "fromInteger" x "Int_Range")

instance Real Int_Range where
  toRational (Int_Range x) = toRational x

instance Enum Int_Range where
  succ (Int_Range x)
    | x < maxBound = Int_Range (succ x)
    | otherwise = error (out_1 "succ" x "Int_Range")
  pred (Int_Range x)
    | x > minBound = Int_Range (pred x)
    | otherwise = error (out_1 "pred" x "Int_Range")
  toEnum x = Int_Range (fromIntegral x)
  fromEnum (Int_Range x) = fromIntegral x

instance Integral Int_Range where
  (Int_Range x) `quot` (Int_Range y) = Int_Range (x `quot` y)
  (Int_Range x) `rem` (Int_Range y) = Int_Range (x `rem` y)
  (Int_Range x) `div` (Int_Range y) = Int_Range (x `div` y)
  (Int_Range x) `mod` (Int_Range y) = Int_Range (x `mod` y)
  x `quotRem` y = (x `quot` y, x `rem` y)
  x `divMod` y = (x `div` y, x `mod` y)
  toInteger (Int_Range x) = fromIntegral x

instance Show Int_Range where
  show (Int_Range x) = show x

instance Read Int_Range where
  readsPrec i str = [(from_Int_to_Int_Range result, rest)]
    where
      [(result, rest)] = readsPrec i str

instance Random Int_Range where
  random gen = (Int_Range x, gen')
    where
      (x, gen') = random gen
  randomR (Int_Range from, Int_Range to) gen = (Int_Range x, gen')
    where
      (x, gen') = randomR (from, to) gen

instance Binary Int_Range where
  put (Int_Range x) = put (fromIntegral x :: Int64)
  get = fmap fromIntegral (get :: Get Int64)

-- Instances for Nat
instance Bounded Nat where
  minBound = Nat 0
  maxBound = Nat maxBound

instance Num Nat where
  (Nat x) + (Nat y)
    | y <= maxBound - x = Nat i_sum
    | otherwise = error (out_2 x "+" y "Nat")
    where
      i_sum = x + y
  (Nat x) - (Nat y)
    | y <= x = Nat diff
    | otherwise = error (out_2 x "-" y "Nat")
    where
      diff = x - y
  (Nat x) * (Nat y)
    | prod <= toInteger (maxBound :: Int) = Nat (fromInteger prod)
    | otherwise = error (out_2 x "*" y "Nat")
    where
      prod = toInteger x * toInteger y
  negate (Nat x)
    | x == 0 = Nat x
    | otherwise = error (out_1 "negate" x "Nat")
  abs (Nat x)
    | check_Nat abs_x = Nat abs_x
    | otherwise = error (out_1 "abs" x "Nat")
    where
      abs_x = abs x
  signum (Nat x)
    | check_Nat sign_x = Nat sign_x
    | otherwise = error (out_1 "signum" x "Nat")
    where
      sign_x = signum x
  fromInteger x
    | check_Natural x && x <= toInteger (maxBound :: Int) = Nat (fromInteger x)
    | otherwise = error (out_1 "fromInteger" x "Nat")

instance Real Nat where
  toRational (Nat x)
    | check_Nat x = toRational x
    | otherwise = error (out_1 "toRational" x "Nat")

instance Enum Nat where
  succ (Nat x)
    | x < maxBound = Nat (succ x)
    | otherwise = error (out_1 "succ" x "Nat")
  pred (Nat x)
    | check_Nat pred_x = Nat pred_x
    | otherwise = error (out_1 "pred" x "Nat")
    where
      pred_x = pred x
  toEnum x
    | check_Natural (toInteger x) = Nat (fromIntegral x)
    | otherwise = error (out_1 "toEnum" x "Nat")
  fromEnum (Nat x)
    | check_Nat x = fromIntegral x
    | otherwise = error (out_1 "fromEnum" x "Nat")

instance Integral Nat where
  (Nat x) `quot` (Nat y)
    | check_Nat x_quot_y = Nat x_quot_y
    | otherwise = error (out_2 x "quot" y "Nat")
    where
      x_quot_y = x `quot` y
  (Nat x) `rem` (Nat y)
    | check_Nat x_rem_y = Nat x_rem_y
    | otherwise = error (out_2 x "rem" y "Nat")
    where
      x_rem_y = x `rem` y
  (Nat x) `div` (Nat y)
    | check_Nat x_div_y = Nat x_div_y
    | otherwise = error (out_2 x "div" y "Nat")
    where
      x_div_y = x `div` y
  (Nat x) `mod` (Nat y)
    | check_Nat x_mod_y = Nat x_mod_y
    | otherwise = error (out_2 x "mod" y "Nat")
    where
      x_mod_y = x `mod` y
  x `quotRem` y = (x `quot` y, x `rem` y)
  x `divMod` y = (x `div` y, x `mod` y)
  toInteger (Nat x) = fromIntegral x

instance Show Nat where
  show (Nat x) = show x

instance Read Nat where
  readsPrec i str = [(from_Int_to_Nat result, rest)]
    where
      [(result, rest)] = readsPrec i str

instance Random Nat where
  random gen = (Nat x, gen')
    where
      (x, gen') = randomR (0, maxBound :: Int) gen
  randomR (Nat from, Nat to) gen = (Nat x, gen')
    where
      (x, gen') = randomR (from, to) gen

instance Binary Nat where
  put (Nat x) = put (fromIntegral x :: Int64)
  get = fmap fromIntegral (get :: Get Int64)

-- Instances for Pos
instance Bounded Pos where
  minBound = Pos 1
  maxBound = Pos maxBound

instance Num Pos where
  (Pos x) + (Pos y)
    | y <= maxBound - x = Pos i_sum
    | otherwise = error (out_2 x "+" y "Pos")
    where
      i_sum = x + y
  (Pos x) - (Pos y)
    | y < x = Pos diff
    | otherwise = error (out_2 x "-" y "Pos")
    where
      diff = x - y
  (Pos x) * (Pos y)
    | prod <= toInteger (maxBound :: Int) = Pos (fromInteger prod)
    | otherwise = error (out_2 x "*" y "Pos")
    where
      prod = toInteger x * toInteger y
  negate (Pos x) = error (out_1 "negate" x "Pos")
  abs (Pos x)
    | check_Pos abs_x = Pos abs_x
    | otherwise = error (out_1 "abs" x "Pos")
    where
      abs_x = abs x
  signum (Pos x)
    | check_Pos sign_x = Pos sign_x
    | otherwise = error (out_1 "signum" x "Pos")
    where
      sign_x = signum x
  fromInteger x
    | check_Positive x && x <= toInteger (maxBound :: Int) = Pos (fromInteger x)
    | otherwise = error (out_1 "fromInteger" x "Pos")

instance Real Pos where
  toRational (Pos x)
    | check_Pos x = toRational x
    | otherwise = error (out_1 "toRational" x "Pos")

instance Enum Pos where
  succ (Pos x)
    | x < maxBound = Pos succ_x
    | otherwise = error (out_1 "succ" x "Pos")
    where
      succ_x = succ x
  pred (Pos x)
    | check_Pos pred_x = Pos pred_x
    | otherwise = error (out_1 "pred" x "Pos")
    where
      pred_x = pred x
  toEnum x
    | check_Positive (toInteger x) = Pos (fromIntegral x)
    | otherwise = error (out_1 "toEnum" x "Pos")
  fromEnum (Pos x)
    | check_Pos x = fromIntegral x
    | otherwise = error (out_1 "fromEnum" x "Pos")

instance Integral Pos where
  (Pos x) `quot` (Pos y)
    | check_Pos x_quot_y = Pos x_quot_y
    | otherwise = error (out_2 x "quot" x "Pos")
    where
      x_quot_y = x `quot` y
  (Pos x) `rem` (Pos y)
    | check_Pos x_rem_y = Pos x_rem_y
    | otherwise = error (out_2 x "rem" x "Pos")
    where
      x_rem_y = x `rem` y
  (Pos x) `div` (Pos y)
    | check_Pos x_div_y = Pos x_div_y
    | otherwise = error (out_2 x "div" x "Pos")
    where
      x_div_y = x `div` y
  (Pos x) `mod` (Pos y)
    | check_Pos x_mod_y = Pos x_mod_y
    | otherwise = error (out_2 x "mod" x "Pos")
    where
      x_mod_y = x `mod` y
  x `quotRem` y = (x `quot` y, x `rem` y)
  x `divMod` y = (x `div` y, x `mod` y)
  toInteger (Pos x) = fromIntegral x

instance Show Pos where
  show (Pos x) = show x

instance Read Pos where
  readsPrec i str = [(from_Int_to_Pos result, rest)]
    where
      [(result, rest)] = readsPrec i str

instance Random Pos where
  random gen = (Pos x, gen')
    where
      (x, gen') = randomR (1, maxBound :: Int) gen
  randomR (Pos from, Pos to) gen = (Pos x, gen')
    where
      (x, gen') = randomR (from, to) gen

instance Binary Pos where
  put (Pos x) = put (fromIntegral x :: Int64)
  get = fmap fromIntegral (get :: Get Int64)

-- Error messages
-- Out of range error message for a single argument function with
-- f being the name of the function
-- x being the argument which triggers the error
-- t being the name of the type which has been broken
out_1 :: (Show a) => String -> a -> String -> String
out_1 f x t = "`" ++ f ++ "` " ++ show x ++ ": Out of range for " ++ t

-- Out of range error message for a two argument function with
-- f being the name of the function
-- x, y being the arguments which trigger the error
-- t being the name of the type which has been broken
out_2 :: (Show a) => a -> String -> a -> String -> String
out_2 x f y t =
  show x ++ " `" ++ f ++ "` " ++ show y ++ ": Out of range for " ++ t
