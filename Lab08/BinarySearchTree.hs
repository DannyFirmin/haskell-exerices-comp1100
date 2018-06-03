module BinarySearchTree (
  BinarySearchTree,
  treeIsValid,        -- :: (Ord a) => BinarySearchTree a -> Bool
  treeContains,       -- :: (Ord a) => a -> BinarySearchTree a -> Bool
  treeInsert,         -- :: (Ord a) => a -> BinarySearchTree a -> BinarySearchTree a
  treeMinimum,        -- :: BinarySearchTree a -> a
  treeMaximum,        -- :: BinarySearchTree a -> a
  treeFlattenOrdered  -- :: BinarySearchTree a -> [a]
) where


import BinaryTree

type BinarySearchTree a = BinaryTree a

isAscending :: Ord a=> [a] -> Bool
isAscending [] = False
isAscending [x] = True
isAscending (x:xs) = x <= head xs && isAscending xs

treeIsValid :: Ord a=>BinarySearchTree a -> Bool
--treeIsValid :: Ord a => BinarySearchTree a -> Bool
treeIsValid = isAscending . treeFlattenOrdered

treeMinimum :: (Ord a)=>BinarySearchTree a -> a
treeMinimum n = case n of
  Null -> error "empty tree"
  Node a Null Null -> a
  Node a Null r -> min a (treeMinimum r)
  Node a l Null -> min a (treeMinimum l)
  Node a l r -> minimum [a, treeMinimum l, treeMinimum r]

treeMaximum :: (Ord a)=> BinarySearchTree a -> a
treeMaximum n = case n of
  Null -> error "empty tree"
  Node a Null Null -> a
  Node a Null r -> max a (treeMaximum r)
  Node a l Null -> max a (treeMaximum l)
  Node a l r -> maximum [a, treeMaximum l, treeMaximum r]


treeContains :: (Eq a) => a -> BinarySearchTree a -> Bool
treeContains a Null = False
treeContains a (Node c l r)
  |a==c = True
  |otherwise = treeContains a l || treeContains a r

treeFlattenOrdered :: BinarySearchTree a -> [a]
treeFlattenOrdered  Null = []
treeFlattenOrdered (Node y l r) = treeFlattenOrdered l ++ [y] ++ treeFlattenOrdered r

-- treeInsert :: Ord a => a -> BinarySearchTree a -> BinarySearchTree a
-- treeInsert = undefined

treeInsert :: Ord a => a -> BinarySearchTree a -> BinarySearchTree a
treeInsert x Null  = Node x Null Null
treeInsert x (Node a l r)
  |a == x = Node a l r
  |a < x = Node a l (treeInsert x r)
  |a > x = Node a (treeInsert x l) r


-- Test
test4 :: BinarySearchTree Char
test4 = Node 'g' (Node 'c' (Node 'a' Null (Node 'b' Null Null))
    (Node 'e' (Node 'f' Null Null) (Node 'g' Null Null)))
    (Node 'i' (Node 'h' Null Null) (Node 'j' Null Null))
test5 :: BinarySearchTree Integer
test5 = Node 92 (Node 11 Null Null) (Node 101 Null (Node 102 Null Null))
