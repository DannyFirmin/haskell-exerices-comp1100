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

tree1 :: BinaryTree Int
tree1 =
  Node
    5
    (Node
       4
       (Node 2 Null (Node 11 Null Null))
       (Node 1 (Node 0 Null Null) (Node (-3) Null Null)))
    (Node 3 (Node 8 (Node (-4) Null Null) (Node 7 Null Null)) Null)

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
