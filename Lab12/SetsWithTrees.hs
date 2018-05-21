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
import BinarySearchTree
import Data.List

newtype Set a = Set{storage::BinarySearchTreeSet a}


data BinarySearchTreeSet a
  = Null
  | Node a
         (BinarySearchTreeSet a)
         (BinarySearchTreeSet a)
  deriving (Show, Eq)

isLegalSet :: (Eq a) => Set a -> Bool
isLegalSet (Set a) = a == nub a

emptySet :: Set a
emptySet = Set Null

singletonSet :: a -> Set a
singletonSet element = Set (Node element Null Null)

setSize :: Integral b => Set a -> b
setSize (Set tree) =
 case tree of
 Null -> 0
 Node _ l r -> 1 + setSize (Set l) + setSize (Set r)

--O(n logn)
setEquals :: (Eq a) => Set a -> Set a -> Bool
setA `setEquals` setB =
  case (storage setA, storage setB) of
    (Null, Null) -> True
    (Null, _) -> False
    (_, Null) -> False
    (Node n l r, Node n' l' r') ->
      n == n' && Set l `setEquals` Set l' && Set r `setEquals` Set r'

containsElement :: (Eq a) => Set a -> a -> Bool
containsElement (Set (Node n l r)) elem
  |n==elem = True
  |otherwise = containsElement (Set l) elem || containsElement (Set r) elem


addElement :: (Eq a) => a -> Set a -> Set a
addElement x (Set Null)  = Set (Node x Null Null)
addElement x Set(Node a l r)
  |a == x = Set$Node a l r
  |a < x = Set$Node a l (addElement x r)
  |a > x = Set$Node a (addElement x l) r

removeElement :: (Eq a) => a -> Set a -> Set a
removeElement element (Set list) = Set (delete element list)


setUnion :: (Eq a) => Set a -> Set a -> Set a
setUnion (Set list_a) (Set list_b) = Set (list_a `union` list_b)

setIntersection :: (Eq a) => Set a -> Set a -> Set a
setIntersection (Set list_a) (Set list_b) = Set (list_a `intersect` list_b)

setDifference :: (Eq a) => Set a -> Set a -> Set a
setDifference (Set list_a) (Set list_b) = Set (list_a \\ list_b)

setMap :: (Eq b) => (a -> b) -> Set a -> Set b
setMap f (Set (Node n l r)) = Set (nub (map f (treeFlattenOrdered(Node n l r))))

setFilter :: (a -> Bool) -> Set a -> Set a
setFilter f (Set list) = Set (filter f list)
