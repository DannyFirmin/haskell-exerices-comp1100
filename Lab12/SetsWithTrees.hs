module SetsWithTrees
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

newtype Set a = Set ()

isLegalSet :: (Eq a) => Set a -> Bool
isLegalSet = undefined

emptySet :: Set a
emptySet = undefined

singletonSet :: a -> Set a
singletonSet = undefined

setSize :: Integral b => Set a -> b
setSize = undefined

setEquals :: (Eq a) => Set a -> Set a -> Bool
setEquals = undefined

containsElement :: (Eq a) => Set a -> a -> Bool
containsElement = undefined

addElement :: (Eq a) => a -> Set a -> Set a
addElement = undefined

removeElement :: (Eq a) => a -> Set a -> Set a
removeElement = undefined

setUnion :: (Eq a) => Set a -> Set a -> Set a
setUnion = undefined

setIntersection :: (Eq a) => Set a -> Set a -> Set a
setIntersection = undefined

setDifference :: (Eq a) => Set a -> Set a -> Set a
setDifference = undefined

setMap :: (Eq b) => (a -> b) -> Set a -> Set b
setMap = undefined

setFilter :: (a -> Bool) -> Set a -> Set a
setFilter = undefined
