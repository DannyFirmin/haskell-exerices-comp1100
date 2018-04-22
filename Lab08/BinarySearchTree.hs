module BinarySearchTree where

import BinaryTree

type BinarySearchTree a = BinaryTree a

treeIsValid :: Ord a => BinarySearchTree a -> Bool
treeIsValid = undefined -- TODO

treeMinimum :: Ord a => BinarySearchTree a -> a
treeMinimum = undefined -- TODO

treeMaximum :: Ord a => BinarySearchTree a -> a
treeMaximum = undefined -- TODO

treeContains :: Ord a => a -> BinarySearchTree a -> Bool
treeContains = undefined -- TODO

treeFlattenOrdered :: Ord a => BinarySearchTree a -> [a]
treeFlattenOrdered = undefined -- TODO

treeInsert :: Ord a => a -> BinarySearchTree a -> BinarySearchTree a
treeInsert = undefined -- TODO
