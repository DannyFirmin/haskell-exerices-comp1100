module MaybeArithmetic where

-- | maybeSqrt
--
-- Examples:
--
-- >>> maybeSqrt (Just 0)
-- Just 0.0
--
-- >>> maybeSqrt (Just 1)
-- Just 1.0
--
-- >>> maybeSqrt (Just (-1))
-- Nothing
maybeSqrt :: Maybe Float -> Maybe Float
maybeSqrt mx = case mx of
  Just x
    | x >= 0 -> Just (sqrt x)
    | otherwise -> Nothing
  Nothing -> Nothing

-- | maybeDiv
--
-- Examples:
--
-- >>> maybeDiv (Just 5) (Just 2)
-- Just 2
--
-- >>> maybeDiv (Just (-5)) (Just 2)
-- Just (-3)
--
-- >>> maybeDiv (Just (-5)) (Just 0)
-- Nothing
--
-- >>> maybeDiv Nothing (Just 1)
-- Nothing
--
-- >>> maybeDiv (Just 1) Nothing
-- Nothing
maybeDiv :: Maybe Integer -> Maybe Integer -> Maybe Integer
maybeDiv mx my = undefined
