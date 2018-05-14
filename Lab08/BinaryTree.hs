module BinaryTree (
      BinaryTree (Null, Node),
      treeSize,     -- :: Integral b => BinaryTree a -> b
      treeDepth,    -- :: Integral b => BinaryTree a -> b
      treeFlatten,  -- :: BinaryTree a -> [a]
      treeLeaves,    -- :: BinaryTree a -> [a]
      treeMap        -- :: (a->b) -> BinaryTree a - > BinaryTree b
) where
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
treeSize n = case n of
  Null -> 0
  Node _ l r -> 1 + treeSize l + treeSize r

treeDepth :: Integral b => BinaryTree a -> b
treeDepth  n = case n of
  Null -> 0
  Node _ l r -> 1 + max(treeDepth l) (treeDepth r)

treeFlatten :: BinaryTree a -> [a]
treeFlatten n = case n of
  Null -> []
  Node a l r -> a:treeFlatten l++treeFlatten r

treeLeaves :: BinaryTree a -> [a]
treeLeaves n = case n of
  Null -> []
  Node a (Null) (Null) -> [a]
  Node _ l r -> treeLeaves l ++ treeLeaves r

-- Test
test1, test2 :: BinaryTree Float
test1 = Node 0.0 Null Null
test2 = Node 10.9 Null (Node (-1.0) Null Null)
test3 :: BinaryTree String
test3 = Node "Galvanic" (Node "metal" (Node "beats" Null Null)
    (Node "stomp" Null Null)) (Node "out" Null (Node "louder" Null
        Null))

-- Exercise 2
treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
treeMap f Null = Null
treeMap f (Node x t1 t2) = Node (f x) (treeMap f t1) (treeMap f t2)