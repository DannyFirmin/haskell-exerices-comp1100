module RoseTree where

data RoseTree a =
  RoseNode a
           [RoseTree a]
  deriving (Show, Eq)

treeSize :: Integral b => RoseTree a -> b
treeSize = undefined -- TODO

treeDepth :: Integral b => RoseTree a -> b
treeDepth = undefined -- TODO

treeLeaves :: RoseTree a -> [a]
treeLeaves = undefined -- TODO
