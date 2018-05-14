module BinaryTreeTests where

import BinaryTree
import Test.QuickCheck

instance Arbitrary a => Arbitrary (BinaryTree a) where
  arbitrary = oneof [pure Null, Node <$> arbitrary <*> arbitrary <*> arbitrary]

-- | A binary tree has size equal to its length when flattened
-- prop> prop_unflattenedSize
prop_unflattenedSize :: BinaryTree String -> Bool
prop_unflattenedSize tree = length (treeFlatten tree) == treeSize tree

-- | Mapping over a binary tree creates a binary tree of the same size
-- prop> prop_keepSize
prop_keepSize :: BinaryTree Integer -> Bool
prop_keepSize tree = (treeSize tree :: Integer) == treeSize (treeMap (+ 1) tree)

-- | Mapping a function over a binary tree and the reverse of that function
--   over the result produces the original binary tree
-- prop> prop_reverseFunction
prop_reverseFunction :: BinaryTree Integer -> Bool
prop_reverseFunction tree = tree == treeMap (+ (-1)) (treeMap (+ 1) tree)

-- | A binary search tree has size no less than its depth
-- prop> prop_largerThanDeep
prop_largerThanDeep :: BinaryTree Integer -> Bool
prop_largerThanDeep tree = (treeSize tree :: Integer) >= treeDepth tree

-- QuickCheck tests to run by hand
test_prop_unflattenedSize :: IO ()
test_prop_unflattenedSize = quickCheck prop_unflattenedSize

test_prop_keepSize :: IO ()
test_prop_keepSize = quickCheck prop_keepSize

test_prop_reverseFunction :: IO ()
test_prop_reverseFunction = quickCheck prop_reverseFunction

test_prop_largerThanDeep :: IO ()
test_prop_largerThanDeep = quickCheck prop_largerThanDeep
