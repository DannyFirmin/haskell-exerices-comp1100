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

isLegalSet :: (Eq a) => Set a -> Bool
isLegalSet (Set list) = list == nub list

emptySet :: Set a
emptySet = Set []

singletonSet :: a -> Set a
singletonSet element = Set [element]

setSize :: Integral b => Set a -> b
setSize (Set list) =
  case list of
    [] -> 0
    _:xs -> 1 + setSize (Set xs)

setEquals :: (Eq a) => Set a -> Set a -> Bool
setA `setEquals` setB =
  case (storage setA, storage setB) of
    ([], []) -> True
    ([], _) -> False
    (_, []) -> False
    (x:xs, _) ->
      containsElement setB x && Set xs `setEquals` removeElement x setB

containsElement :: (Eq a) => Set a -> a -> Bool
containsElement (Set list) element = element `elem` list

addElement :: (Eq a) => a -> Set a -> Set a
addElement element set = Set (element : storage (removeElement element set))

removeElement :: (Eq a) => a -> Set a -> Set a
removeElement element (Set list) = Set (delete element list)

setUnion :: (Eq a) => Set a -> Set a -> Set a
setUnion (Set list_a) (Set list_b) = Set (list_a `union` list_b)

setIntersection :: (Eq a) => Set a -> Set a -> Set a
setIntersection (Set list_a) (Set list_b) = Set (list_a `intersect` list_b)

setDifference :: (Eq a) => Set a -> Set a -> Set a
setDifference (Set list_a) (Set list_b) = Set (list_a \\ list_b)

setMap :: (Eq b) => (a -> b) -> Set a -> Set b
setMap f (Set list) = Set (nub (map f list))

setFilter :: (a -> Bool) -> Set a -> Set a
setFilter f (Set list) = Set (filter f list)
