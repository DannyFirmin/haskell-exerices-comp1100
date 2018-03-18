module Lists where

-- TODO: Fix the following errors by rewriting each expression
string1 = "A" : " reasonably broken string"
string2 = [1,2,3] : [4,5,6] : [7,8,9]
string3 = (4.0, "Teddy") : (pi, "Duck")
string4 = 9:8:[]:7:6:[]:[]

-- TODO: Fix the following errors only by adding new characters
string1' = "A" : " reasonably broken string"
string2' = [1,2,3] : [4,5,6] : [7,8,9]
string3' = (4.0, "Teddy") : (pi, "Duck")
string4' = 9:8:[]:7:6:[]:[]

-- |
-- Examples:
--
-- >>> deconstructList []
-- (Nothing,[])
--
-- >>> deconstructList [1]
-- (Just 1,[])
--
-- >>> deconstructList [1,2]
-- (Just 1,[2])
--
deconstructList :: [a] -> (Maybe a, [a])
deconstructList list = case list of
  []   -> (Nothing, [])
  x:xs -> (Just x, xs)

-- | deconstructString is a version of deconstructList specialised for String values
-- Examples:
--
-- >>> deconstructString ""
-- (Nothing,"")
--
-- >>> deconstructString "a"
-- (Just 'a',"")
--
-- >>> deconstructString "ab"
-- (Just 'a',"b")
--
-- prop> deconstructString x == deconstructList x
--
deconstructString :: String -> (Maybe Char, String)
deconstructString string = case string of
  ""   -> (Nothing, "")
  c:cs -> (Just c, cs)

-- | A String implementation from scratch
data MyString = EmptyString | ConsStr Char MyString deriving Show

-- |
-- Examples:
--
-- >>> deconstructMyString EmptyString
-- (Nothing,EmptyString)
--
-- >>> deconstructMyString (ConsStr 'a' EmptyString)
-- (Just 'a',EmptyString)
--
-- >>> deconstructMyString (ConsStr 'a' (ConsStr 'b' EmptyString))
-- (Just 'a',ConsStr 'b' EmptyString)
--
deconstructMyString :: MyString -> (Maybe Char, MyString)
deconstructMyString = undefined -- TODO

-- An equivalent list implementation from scratch
data MyList a = EmptyList | Cons a (MyList a) deriving Show

-- |
-- Examples:
--
-- >>> deconstructMyList EmptyList
-- (Nothing,EmptyList)
--
-- >>> deconstructMyList (Cons 1 EmptyList)
-- (Just 1,EmptyList)
--
-- >>> deconstructMyList (Cons 1 (Cons 2 EmptyList))
-- (Just 1,Cons 2 EmptyList)
--
deconstructMyList :: MyList a -> (Maybe a, MyList a)
deconstructMyList = undefined -- TODO

-- |
-- Examples:
--
-- >>> swapFirstTwoElements [1,2]
-- [2,1]
--
-- >>> swapFirstTwoElements ["a","b"]
-- ["b","a"]
--
-- >>> swapFirstTwoElements "abcd"
-- "bacd"
swapFirstTwoElements :: [a] -> [a]
swapFirstTwoElements = undefined -- TODO

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
--
evalStringExpression :: String -> String
evalStringExpression s = showExpression (eval (tokenise s))

-- |
-- Examples:
--
-- >> eval [Num 3.2, Plus, Minus, Num 4.2, Minus, Num 5.3, Plus, Num 6.3]
-- [Num 0.0]
--
-- Start by writing a type signature in place of the next commented line:
-- eval :: ...your code here...
eval = undefined -- TODO