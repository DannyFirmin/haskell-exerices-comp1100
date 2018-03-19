module Lists where

-- TODO: Fix the following errors by rewriting each expression
list1 = "A" : " reasonably broken string"
list2 = [1,2,3] : [4,5,6] : [7,8,9]
list3 = (4.0, "Teddy") : (pi, "Duck")
list4 = 9:8:[]:7:6:[]:[]

-- TODO: Fix the following errors only by adding new characters
list1' = "A" : " reasonably broken string"
list2' = [1,2,3] : [4,5,6] : [7,8,9]
list3' = (4.0, "Teddy") : (pi, "Duck")
list4' = 9:8:[]:7:6:[]:[]

-- | deconstruct separates a list into its head and tail.
-- | The head of an empty list is Nothing, and its tail is the empty list.
-- Examples:
--
-- >>> deconstruct []
-- (Nothing,[])
--
-- >>> deconstruct [1]
-- (Just 1,[])
--
-- >>> deconstruct [1,2]
-- (Just 1,[2])
--
deconstruct :: [a] -> (Maybe a, [a])
deconstruct list = case list of
  []   -> (Nothing, [])
  x:xs -> (Just x, xs)

-- | swapFirstTwoElements
--
-- Examples:
-- >>> swapFirstTwoElements [1,2]
-- [2,1]
-- >>> swapFirstTwoElements ["a","b"]
-- ["b","a"]
-- >>> swapFirstTwoElements "abcd"
-- "bacd"
swapFirstTwoElements :: [a] -> [a]
swapFirstTwoElements = undefined -- TODO

-- | oddsAndEvens
--
-- Examples:
-- >>> oddsAndEvens []
-- (0,0)
-- >>> oddsAndEvens [0]
-- (0,1)
-- >>> oddsAndEvens [1]
-- (1,0)
-- >>> oddsAndEvens [1,2]
-- (1,1)
-- >>> oddsAndEvens [1,2,3]
-- (2,1)
--
-- prop> oddsAndEvens [1..n | n/2 == 0]
oddsAndEvens :: [Integer] -> (Int,Int)
oddsAndEvens = undefined -- TODO

--
-- Pattern matching and evaluation
--
type Expression = [Token]
data Token = Plus | Minus | Times | DividedBy | Power | Num {number :: Double}
    deriving (Show, Eq)

tokenise :: String -> Expression
tokenise ""     = []
tokenise (c:cs) = case c of
  '+' -> Plus      : tokenise cs
  '-' -> Minus     : tokenise cs
  '*' -> Times     : tokenise cs
  '/' -> DividedBy : tokenise cs
  '^' -> Power     : tokenise cs
  _ | c `elem` ['0' .. '9'] -> case reads (c:cs) of
          [(value, rest)] -> Num value : tokenise rest
          _               -> error "Could not read number"
    | c `elem` [' ', '\t'] -> tokenise cs
    | otherwise -> error "Unknown Symbol"

showExpression :: Expression -> String
showExpression []     = ""
showExpression (e:es) = case e of
    Plus      -> " + "  ++ showExpression es
    Minus     -> " - "  ++ showExpression es
    Times     -> " * "  ++ showExpression es
    DividedBy -> " / "  ++ showExpression es
    Power     -> " ^ "  ++ showExpression es
    Num x     -> show x ++ showExpression es

-- |
-- Examples:
--
-- >>> evalStringExpression "3.2 + -4.2 - 5.3 + 6.3"
-- 0.0
evalStringExpression :: String -> String
evalStringExpression s = showExpression (eval (tokenise s))

-- |
-- Examples:
--
-- >> eval [Num 3.2, Plus, Minus, Num 4.2, Minus, Num 5.3, Plus, Num 6.3]
-- [Num 0.0]
--
-- Start by writing a better type signature before giving a definition of eval:
eval :: a -- TODO
eval = undefined -- TODO
