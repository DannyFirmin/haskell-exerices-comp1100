module CustomLists where

-- | A list of Int elements
data Ints = NoInts | SomeInts Int Ints
  deriving Show

-- Examples:
ints :: Ints
ints = SomeInts 1 (SomeInts 2 (SomeInts 3 NoInts))

-- | noInts
-- Examples:
-- >>> noInts NoInts
-- True
-- >>> noInts ints
-- False
noInts :: Ints -> Bool
noInts = undefined -- TODO

-- | headInt
-- Examples:
-- The first example should return an error or undefined:
-- >>> headInt NoInts
-- ...
-- >>> headInt ints
-- 1
headInt :: Ints -> Int
headInt = undefined -- TODO

-- | tailInts
-- Examples:
-- The first example should return an error or undefined:
-- >>> tailInts NoInts
-- ...
-- >>> tailInts (SomeInts 1 NoInts)
-- NoInts
-- >>> tailInts ints
-- SomeInts 2 (SomeInts 3 NoInts)
tailInts :: Ints -> Ints
tailInts = undefined -- TODO

-- | A list of Char elements
data Chars = NoChars | SomeChars Char Chars
  deriving Show

chars :: Chars
chars = SomeChars 'c' (SomeChars 'a' (SomeChars 't' NoChars))

-- | noChars
-- Examples:
-- >>> noChars NoChars
-- True
-- >>> noChars (SomeChars 'a' NoChars)
-- False
noChars :: Chars -> Bool
noChars = undefined -- TODO

-- | headChar
-- Examples:
-- The first example should return an error or undefined:
-- >>> headChar NoChars
-- ...
-- >>> headChar (SomeChars 'a' NoChars)
-- 'a'
-- >>> headChar (tailChars (SomeChars 'c' (SomeChars 'a' NoChars)))
-- 'a'
headChar :: Chars -> Char
headChar = undefined -- TODO

-- | tailChars
-- Examples:
-- The first example should return an error or undefined:
-- >>> tailChars NoChars
-- ...
-- >>> tailChars (SomeChars 'a' NoChars)
-- NoChars
-- >>> tailChars (SomeChars 'c' (SomeChars 'a' NoChars))
-- SomeChars 'a' NoChars
tailChars :: Chars -> Chars
tailChars = undefined -- TODO

-- | A List of arbitrary `a` elements
data List a = Empty | Cons a (List a)
  deriving Show

intList :: List Int
intList = Cons 1 (Cons 2 (Cons 3 Empty))

charList :: List Char
charList = Cons 'c' (Cons 'a' (Cons 't' Empty))

-- | isEmptyList
-- Examples:
-- >>> isEmptyList Empty
-- True
-- >>> isEmptyList intList
-- False
isEmptyList :: List a -> Bool
isEmptyList = undefined -- TODO

-- | headList
-- Examples:
-- The first example should return an error or undefined:
-- >>> headList Empty
-- ...
-- >>> headList intList
-- 1
-- >>> headList charList
-- 'c'
headList :: List a -> a
headList = undefined -- TODO

-- | tailList
-- Examples:
-- The first example should return an error or undefined:
-- >>> tailList Empty
-- ...
-- >>> tailList (Cons 1 Empty)
-- Empty
-- >>> tailList intList
-- Cons 2 (Cons 3 Empty)
-- >>> headList (tailList intList)
-- 2
-- >>> headList (tailList (tailList intList))
-- 3
-- >>> tailList charList
-- Cons 'a' (Cons 't' Empty)
-- >>> headList (tailList charList)
-- 'a'
-- >>> headList (tailList (tailList charList))
-- 't'
tailList :: List a -> List a
tailList = undefined -- TODO

-- | unconsList
-- Examples:
-- >>> unconsList Empty
-- Nothing
-- >>> unconsList (Cons 1 Empty)
-- Just (1,Empty)
-- >>> unconsList intList
-- Just (1,Cons 2 (Cons 3 Empty))
-- >>> unconsList charList
-- Just ('c',Cons 'a' (Cons 't' Empty))
unconsList :: List a -> Maybe (a, List a)
unconsList = undefined

-- | lastList
-- Examples:
-- The first example should return an error or undefined:
-- >>> lastList Empty
-- ...
-- >>> lastList (Cons 1 Empty)
-- 1
-- >>> lastList intList
-- 3
-- >>> lastList charList
-- 't'
lastList :: List a -> a
lastList = undefined -- TODO

-- | lengthList
-- Examples:
-- >>> lengthList Empty
-- 0
-- >>> lengthList (Cons 1 Empty)
-- 1
-- >>> lengthList intList
-- 3
-- >>> lengthList charList
-- 3
lengthList :: List a -> Int
lengthList = undefined -- TODO

-- | addFirst
-- Examples:
-- >>> addFirst 1 Empty
-- Cons 1 Empty
-- >>> addFirst 2 (addFirst 1 Empty)
-- Cons 2 (Cons 1 Empty)
-- >>> addFirst 0 intList
-- Cons 0 (Cons 1 (Cons 2 (Cons 3 Empty)))
addFirst :: a -> List a -> List a
addFirst x xs = Cons x xs

-- | addLast
-- Examples:
-- >>> addLast 1 Empty
-- Cons 1 Empty
-- >>> addLast 2 (addLast 1 Empty)
-- Cons 1 (Cons 2 Empty)
-- >>> addLast 4 intList
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty)))
addLast :: a -> List a -> List a
addLast = undefined -- TODO

-- | Convert from/to builtin list to/from our custom list
-- prop> fromList (toList l) == l
fromList :: [a] -> List a
fromList [] = Empty
fromList (x:xs) = Cons x (fromList xs)
toList :: List a -> [a]
toList Empty = []
toList (Cons x xs) = x:(toList xs)
