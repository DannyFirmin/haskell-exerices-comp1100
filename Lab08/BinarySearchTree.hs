module BinarySearchTree where

import BinaryTree

type BinarySearchTree a = BinaryTree a

inOrder :: BinarySearchTree a -> [a]
inOrder Null = []
inOrder (Node y l r) = inOrder l ++ [y] ++ inOrder r

isAscending :: [Int] -> Bool
isAscending [] = False
isAscending [x] = True
isAscending (x:xs) = x <= head (xs) && isAscending xs

treeIsValid :: BinarySearchTree Int -> Bool
--treeIsValid :: Ord a => BinarySearchTree a -> Bool
treeIsValid = isAscending . inOrder

treeMinimum :: BinarySearchTree a -> a
treeMinimum = undefined -- TODO

treeMaximum :: BinarySearchTree a -> a
treeMaximum = undefined -- TODO

treeContains :: Ord a => a -> BinarySearchTree a -> Bool
treeContains = undefined -- TODO

treeFlattenOrdered :: BinarySearchTree a -> [a]
treeFlattenOrdered = undefined -- TODO

treeInsert :: Ord a => a -> BinarySearchTree a -> BinarySearchTree a
treeInsert = undefined -- TODO

-- Test
test4 :: BinarySearchTree Char
test4 = Node 'g' (Node 'c' (Node 'a' Null (Node 'b' Null Null))
    (Node 'e' (Node 'f' Null Null) (Node 'g' Null Null)))
    (Node 'i' (Node 'h' Null Null) (Node 'j' Null Null))
test5 :: BinarySearchTree Integer
test5 = Node 92 (Node 11 Null Null) (Node 101 Null (Node 102 Null Null))