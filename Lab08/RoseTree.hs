module RoseTree where

data RoseTree a =
  RoseNode a
           [RoseTree a]
  deriving (Show, Eq)


treeSize :: Integral b => RoseTree a -> b
treeSize rt = case rt of
  RoseNode a (x:xs) -> treeSize x + treeSize (RoseNode a xs)
  RoseNode a [] -> 1

treeDepth :: Integral b => RoseTree a -> b
treeDepth rt = case rt of
  RoseNode _ [] -> 1
  RoseNode _ r -> 1 + maximum (map treeDepth r)

treeLeaves :: RoseTree a -> [a]-- hint: consider the `concatMap` higher order function from the Prelude.
treeLeaves rt = case rt of
  RoseNode a [] -> [a]
  RoseNode _ r -> concatMap treeLeaves r

treeMap :: (a -> b) -> RoseTree a -> RoseTree b
treeMap fun rt = case rt of
  RoseNode a [] -> RoseNode (fun a) []
  RoseNode a r -> RoseNode (fun a) (map(treeMap fun) r)

test :: RoseTree Integer
test = RoseNode 1 [RoseNode 2 [], RoseNode 3 [RoseNode 4 []],
    RoseNode 5 [RoseNode 6 [], RoseNode 7 []]]
