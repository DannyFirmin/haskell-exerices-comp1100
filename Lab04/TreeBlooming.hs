import CodeWorld

tree :: Integer -> Picture -> Picture
tree 0 b = b
tree n b =
  polyline [(0,0),(0,1)] &
  translated 0 1
    (rotated (  pi/10) (tree (n-1) b) &
     rotated (- pi/10) (tree (n-1) b))

-- TODO: Make the tree bloom by using something else in place of 'blank'
myAnimation :: Double -> Picture
myAnimation t = tree 8 blank

main :: IO ()
main = animationOf myAnimation
