module Expression where

data Expression a
  = Number a
  | Node (Expression a)
         Operand
         (Expression a)
  deriving (Show, Eq)

data Operand
  = Plus
  | Minus
  | Times
  | DividedBy
  | Power
  deriving (Show, Eq)

eval :: Floating a => Expression a -> Expression a
eval (Number a) = Number a
eval (Node a Plus b) =  eval a + eval b
eval (Node a Minus b) = eval a - eval b
eval (Node a Times b) = eval a * eval b
eval (Node a Power b) = eval a ^ eval b


