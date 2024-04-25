module Practicum3 where

{-
Name:           amin soleimani
VU-net id:      asi273
Student number: <Student number>
Discussed with: <In case you discussed the exercises with someone else,
                 please mention his/her name(s) explicitly here>
Remarks:        <In case something need special attention,
                 please tell us>
Sources:        <in case you used sources such as books or webpages
                 please mention them here>
-}

-- Exercises Arithmetical Expressions
data IntExp  = Lit Int | Add IntExp IntExp | Mul IntExp IntExp
  deriving Show

showintexp :: IntExp -> String
showintexp e = 
  case e of 
    (Lit num) -> show num
    (Add a b) -> "(" ++ showintexp a ++ "+" ++ showintexp b ++ ")"
    (Mul a b) -> "(" ++ showintexp a ++ "*" ++ showintexp b ++ ")"

evalintexp :: IntExp -> Int
evalintexp e = 
  case e of 
    (Lit num) -> num
    (Add  a  b) ->  evalintexp a + evalintexp  b 
    (Mul  a  b) ->  evalintexp a * evalintexp  b 

-- Exercises Combinatory Logic
data Term = S | K | I | App Term Term

instance Show Term where
  show a = showterm a

showterm :: Term -> String
showterm t = 
  case t of 
    S -> "S"
    K -> "K"
    I -> "I"
    (App t1 t2) -> "(" ++ showterm t1 ++ showterm t2 ++ ")"

isredex :: Term -> Bool
isredex t = 
  case t of
    (App I x) -> True
    (App(App K x)y) -> True
    (App(App(App S x)y)z) -> True
    otherwise -> False

hasredex :: Term -> Bool
hasredex t = if (isredex t == True) then True else
  case t of
    (App K x) -> hasredex x
    (App S x) -> hasredex x
    (App (App x y) z) -> (hasredex (App x y)) || (hasredex z)
    --otherwise -> False 

isnormalform :: Term -> Bool
isnormalform t = 
  case t of
    I -> True
    K -> True
    S -> True
    (App K x) -> isnormalform x
    (App S x) -> isnormalform x
    (App (App S x) y) -> (isnormalform (App S x)) && (isnormalform y)
    otherwise -> False

headstep :: Term -> Term
headstep t = if (isredex t == False) then t else
  case t of
    (App I x) -> x
    (App (App K x) y) -> x
    (App (App (App S x) y) z) -> (App (App x z) (App y z))


doall :: Term -> Term
doall = undefined



-- Exercises Equational Specifications
data Thing = C | D | X
  deriving (Show, Eq, Bounded, Enum)

nxt :: Thing -> Thing
nxt t =
  case t of
    C -> X
    D -> C
    X -> D


data I = ZERO | ONE
  deriving (Show, Eq, Bounded, Enum)

s :: I -> I
s t = 
  case t of
    ZERO -> ONE
    ONE -> ZERO

p :: I -> I
p t = 
  case t of
    ZERO -> ONE
    ONE -> ZERO
