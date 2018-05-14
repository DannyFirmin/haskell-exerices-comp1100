module BinarySearchTreeTests where

import BinarySearchTree
import BinaryTree
import Data.List (nub)
import Test.QuickCheck

instance (Ord a, Arbitrary a) => Arbitrary (BinaryTree a) where
  arbitrary = sized genTree
    where
      genTree n = fmap (foldr treeInsert Null) (vector n)

-- | A BST has size equal to its length when flattened
-- prop> prop_unflattenedSize
prop_unflattenedSize :: BinarySearchTree String -> Bool
prop_unflattenedSize tree = length (treeFlattenOrdered tree) == treeSize tree

-- | A BST has a minimum element that appears first in the flattened tree
-- prop> prop_minElement
prop_minElement :: BinarySearchTree Integer -> Bool
prop_minElement tree =
  tree == Null || treeMinimum tree == head (treeFlattenOrdered tree)

-- | Adding an element to a BST increases its size by one, unless already there
-- prop> prop_addOneElement
prop_addOneElement :: BinarySearchTree Char -> Bool
prop_addOneElement tree
  | treeContains 'a' tree =
    (treeSize tree :: Integer) == treeSize (treeInsert 'a' tree)
  | otherwise = 1 + (treeSize tree :: Integer) == treeSize (treeInsert 'a' tree)

-- | Adding the min and max elements to a BST does not change it
-- prop> prop_addMinMaxElements
prop_addMinMaxElements :: BinarySearchTree Char -> Bool
prop_addMinMaxElements tree =
  tree == Null ||
  tree == treeInsert (treeMaximum tree) (treeInsert (treeMinimum tree) tree)

-- | A BST has distinct elements
-- prop> prop_distinctElements
prop_distinctElements :: String -> Bool
prop_distinctElements list =
  length (nub list) == length (treeFlattenOrdered (insertListToTree list Null))
  where
    insertListToTree list' tree =
      case list' of
        [] -> tree
        x:xs -> treeInsert x (insertListToTree xs tree)

-- | A BST has a number of constraints
-- prop> prop_isValidBinarySearchTree
prop_isValidBinarySearchTree :: BinarySearchTree Char -> Bool
prop_isValidBinarySearchTree tree = checkTreeBounds tree Nothing Nothing
  where
    checkTreeBounds ::
         (Ord a) => BinarySearchTree a -> Maybe a -> Maybe a -> Bool
    checkTreeBounds cTree minE maxE =
      case cTree of
        Null -> True
        Node e left right ->
          minE `maybeLessThan` Just e &&
          Just e `maybeLessThan` maxE &&
          checkTreeBounds left minE (maybeMinmax min (Just e) maxE) &&
          checkTreeBounds right (maybeMinmax max minE (Just e)) maxE
    maybeMinmax :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
    maybeMinmax f a b =
      case (a, b) of
        (Nothing, Nothing) -> Nothing
        (_, Nothing) -> a
        (Nothing, _) -> b
        (Just aValue, Just bValue) -> Just (f aValue bValue)
    maybeLessThan :: (Ord a) => Maybe a -> Maybe a -> Bool
    maybeLessThan a b =
      case (a, b) of
        (Nothing, Nothing) -> True
        (_, Nothing) -> True
        (Nothing, _) -> True
        (Just aValue, Just bValue) -> aValue < bValue

-- | A BST is reasonably balanced
-- prop> prop_reasonablyBalanced
prop_reasonablyBalanced :: BinarySearchTree Integer -> Bool
prop_reasonablyBalanced tree =
  tree == Null ||
  ((treeSize tree :: Integer) < 4) ||
  (fromIntegral (treeDepth tree :: Integer) :: Double) <=
  2 * logBase 2 (1 + fromIntegral (treeSize tree :: Integer))

-- QuickCheck tests to run by hand
test_prop_unflattenedSize :: IO ()
test_prop_unflattenedSize = quickCheck prop_unflattenedSize

test_prop_minElement :: IO ()
test_prop_minElement = quickCheck prop_minElement

test_prop_addOneElement :: IO ()
test_prop_addOneElement = quickCheck prop_addOneElement

test_prop_addMinMaxElements :: IO ()
test_prop_addMinMaxElements = quickCheck prop_addMinMaxElements

test_prop_distinctElements :: IO ()
test_prop_distinctElements = quickCheck prop_distinctElements

test_prop_isValidBinarySearchTree :: IO ()
test_prop_isValidBinarySearchTree = quickCheck prop_isValidBinarySearchTree

test_prop_reasonablyBalanced :: IO ()
test_prop_reasonablyBalanced = quickCheck prop_reasonablyBalanced
