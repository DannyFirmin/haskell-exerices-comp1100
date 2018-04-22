module BinaryTree where

data List a
  = Empty
  | Cons a
         (List a)
  deriving (Show, Eq)

data BinaryTree a
  = Null
  | Node a
         (BinaryTree a)
         (BinaryTree a)
  deriving (Show, Eq)

-- Exercise 1
treeSize :: Integral b => BinaryTree a -> b
treeSize = undefined -- TODO

treeDepth :: Integral b => BinaryTree a -> b
treeDepth = undefined -- TODO

treeFlatten :: BinaryTree a -> [a]
treeFlatten = undefined -- TODO

treeLeaves :: BinaryTree a -> [a]
treeLeaves = undefined -- TODO

-- Exercise 2
treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
treeMap = undefined -- TODO
