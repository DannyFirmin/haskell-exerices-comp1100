module SetsWithLists
  ( Set -- Set a = Set {storage :: [a]}
  , isLegalSet -- :: (Eq a) => Set a -> Bool
  , emptySet -- :: Set a
  , singletonSet -- :: a -> Set a
  , setSize -- :: Integral b => Set a -> b
  , setEquals -- :: (Eq a) => Set a -> Set a -> Bool
  , containsElement -- :: (Eq a) => Set a -> a -> Bool
  , addElement -- :: (Eq a) => a -> Set a -> Set a
  , removeElement -- :: (Eq a) => a -> Set a -> Set a
  , setUnion -- :: (Eq a) => Set a -> Set a -> Set a
  , setIntersection -- :: (Eq a) => Set a -> Set a -> Set a
  , setDifference -- :: (Eq a) => Set a -> Set a -> Set a
  , setMap -- :: (Eq b) => (a -> b) -> Set a -> Set b
  , setFilter -- :: (a -> Bool) -> Set a -> Set a
  ) where

import Data.List

newtype Set a = Set
  { storage :: [a]
  } deriving (Show)
--O(n^2)
isLegalSet :: (Eq a) => Set a -> Bool
isLegalSet (Set list) = list == nub list

--O(1)
emptySet :: Set a
emptySet = Set []

--O(1)
singletonSet :: a -> Set a
singletonSet element = Set [element]

--O(n)
setSize :: Integral b => Set a -> b
setSize (Set list) =
  case list of
    [] -> 0
    _:xs -> 1 + setSize (Set xs)

--O(n^2)
setEquals :: (Eq a) => Set a -> Set a -> Bool
setA `setEquals` setB =
  case (storage setA, storage setB) of
    ([], []) -> True
    ([], _) -> False
    (_, []) -> False
    (x:xs, _) ->
      containsElement setB x && Set xs `setEquals` removeElement x setB

--O(n)
containsElement :: (Eq a) => Set a -> a -> Bool
containsElement (Set list) element = element `elem` list

--O(n)
addElement :: (Eq a) => a -> Set a -> Set a
addElement element set = Set (element : storage (removeElement element set))

--O(n)
removeElement :: (Eq a) => a -> Set a -> Set a
removeElement element (Set list) = Set (delete element list)

--O(n)
setUnion :: (Eq a) => Set a -> Set a -> Set a
setUnion (Set list_a) (Set list_b) = Set (list_a `union` list_b)

--O(n^2)
setIntersection :: (Eq a) => Set a -> Set a -> Set a
setIntersection (Set list_a) (Set list_b) = Set (list_a `intersect` list_b)

--O(n^2)
setDifference :: (Eq a) => Set a -> Set a -> Set a
setDifference (Set list_a) (Set list_b) = Set (list_a \\ list_b)

--O(n^2)
setMap :: (Eq b) => (a -> b) -> Set a -> Set b
setMap f (Set list) = Set (nub (map f list))

--O(n)
setFilter :: (a -> Bool) -> Set a -> Set a
setFilter f (Set list) = Set (filter f list)
