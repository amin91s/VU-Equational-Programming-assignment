{-# LANGUAGE RankNTypes #-}
module Practicum2A where

{-
Name:           Amin Soleimani
VU-net id:      asi273
Student number: 2640338
Discussed with: <In case you discussed the exercises with someone else,
                 please mention his/her name(s) explicitly here>
Remarks:        <In case something need special attention,
                 please tell us>
Sources:        
                 https://karczmarczuk.users.greyc.fr/Essays/church.html
                 https://commandercoriander.net/blog/2016/01/05/solving-the-towers-of-hanoi-with-haskell/
                 http://learnyouahaskell.com/recursion
                 https://stackoverflow.com/questions/2097501/learning-haskell-how-to-remove-an-item-from-a-list-in-haskell

-}

-- -------------------------
-- Exercises Infinite Lists
-- -------------------------

-- Exercise 1
naturals :: [Integer]
naturals = 1: [x+1| x <- naturals]
{-
--using map:
naturals = 1: map (+1) naturals
--using aux functions:
auxNat x = x : (auxNat (x+1))
naturals = auxNat 1
-}

-- Exercise 2
zeroesandones :: [Integer]
zeroesandones = 0:(head zeroesandones)+1:zeroesandones

-- Exercise 3
threefolds :: [Integer]
threefolds = 0: map (*3) naturals
{- Using List comprehension
threefolds = 0:[x | x <- naturals, x `mod` 3 == 0]
-}
-- Exercise 4
removeif :: (a -> Bool) -> [a] -> [a]
removeif p l =
  case l of
    [] -> []
    (h:t) -> if (p h) then removeif p t else h: removeif p t
nothreefolds :: [Integer]
nothreefolds = removeif (\x -> x `mod` 3 == 0) naturals
--nothreefolds = [x | x <- naturals, x `mod` 3 /= 0]

-- Exercise 5
allnfolds :: Integer -> [Integer]
allnfolds n = 0:[x*n | x <- naturals]

-- Exercise 6
allnaturalsexceptnfolds :: Integer -> [Integer]
allnaturalsexceptnfolds n = removeif (\x-> x `mod` n == 0) naturals

-- Exercise 7
allelementsexceptnfolds :: Integer -> [Integer] -> [Integer]
allelementsexceptnfolds n l = removeif (\x-> x `mod` n == 0) l

-- Exercise 8
eratosthenes :: [Integer]
eratosthenes = auxFun (removeif (\x-> x == 1) naturals)
auxFun (h:t) =  h: auxFun (removeif (\x -> x `mod` h == 0) t)
-- Exercise 9
fibonacci :: [Integer]
fibonacci = 0:1: zipWith (+) fibonacci (tail fibonacci)

-- -----------------------
-- Exercise Church Numerals
-- -----------------------
-- we need polymorphic types for the Church Numerals 
type ChurchNumeral = forall a . (a -> a) -> a -> a

-- Exercise 1
churchnumeral :: (Eq a, Num a) => a -> ChurchNumeral 
churchnumeral n =
  if   n == 0
  then \s z -> z
  else \s z -> churchnumeral (n - 1) s (s z)

backtointeger :: (Num a) => ChurchNumeral -> a
backtointeger cn = cn (+1) 0



{- 
--backtointeger (churchnumeral 5) == 5

one = churchnumeral 1
backtointeger one == 1
 -}

-- Exercise 2
churchequality ::  ChurchNumeral  -> ChurchNumeral  -> Bool
churchequality x y = backtointeger x == backtointeger y

-- Exercise 3 given as example
successor ::  ChurchNumeral -> ChurchNumeral
successor x s z  = s ( x s z ) 
 
-- Exercise 4
successorb :: ChurchNumeral -> ChurchNumeral
successorb x s z = x s (s z)

-- Exercise 5
apply1 :: (Eq a, Num a) => (ChurchNumeral-> ChurchNumeral) ->  a -> a
apply1 f n =  backtointeger ( f ( churchnumeral n ) ) 

-- Exercise 6
addition :: ChurchNumeral -> ChurchNumeral -> ChurchNumeral
addition x y s z  = x s (y s z)

multiplication ::  ChurchNumeral -> ChurchNumeral -> ChurchNumeral
multiplication x y s  = x (y s)

exponentiation ::  ChurchNumeral -> ChurchNumeral -> ChurchNumeral 
exponentiation x y  = y x 

-- Exercise 7
apply2 :: (Eq a, Num a) => (ChurchNumeral -> ChurchNumeral -> ChurchNumeral) -> a -> a -> a
apply2 f m n  = backtointeger (f (churchnumeral m) (churchnumeral n))


-- ---------------------
-- Exercises Binary Trees
-- ---------------------
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)

-- Exercise 1
numberofnodes :: BinaryTree a -> Integer
numberofnodes t =
   case t of
     Leaf -> 0
     Node left m right -> 1 + (numberofnodes left) + (numberofnodes right)

-- Exercise 2
height :: BinaryTree a -> Integer
height t = 
  case t of 
    Leaf -> 0
    Node left m right | height left < height right -> height right + 1
                      | otherwise -> height left + 1
-- Exercise 3
sumnodes :: (Num a) => BinaryTree a -> a
sumnodes t =
  case t of
    Leaf -> 0
    Node left parent right -> (sumnodes left) + parent + (sumnodes right) 

-- Exercise 4
mirror :: BinaryTree a -> BinaryTree a
mirror t = 
  case t of
    Leaf -> Leaf
    Node left mid right -> Node (mirror right) mid (mirror left)

-- Exercise 5
flatten :: BinaryTree a -> [a]
flatten t = 
  case t of
    Leaf -> []
    Node left mid right -> (flatten left) ++ [mid] ++ (flatten right)

-- Exercise 6
treemap :: (a -> b) -> BinaryTree a -> BinaryTree b
treemap f t = 
  case t of
    Leaf -> Leaf
    Node left mid right -> Node (treemap f left) (f mid) (treemap f right)

-- -------------------------
-- Exercises Binary Search Trees
-- -------------------------

-- Exercise 1
smallerthan :: (Ord a) => a -> BinaryTree a -> Bool
smallerthan e t =
  case t of
    Leaf -> True
    Node left mid right -> (e > mid) && (smallerthan e left) && (smallerthan e right)

largerthan :: (Ord a) => a -> BinaryTree a -> Bool
largerthan e t =
  case t of
    Leaf -> True
    Node left mid right -> (e < mid) && (largerthan e left) && (largerthan e right)


-- Exercise 2
isbinarysearchtree :: (Ord a) => BinaryTree a -> Bool
isbinarysearchtree t = 
  case t of
    Leaf -> True
    Node left mid right -> (smallerthan mid right) && (largerthan mid left) && (isbinarysearchtree left) && (isbinarysearchtree right)

-- Exercise 3
iselement :: (Ord a, Eq a) => a -> BinaryTree a -> Bool
iselement n t =
  case t of
    Leaf -> False
    Node left mid right -> (n == mid ) || (if (n < mid) then (iselement n left) else (iselement n right))
                        
                         

-- Exercise 4
insert :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
insert e t = 
  case t of
    Leaf ->  Node (Leaf) e (Leaf)
    Node left mid right    | iselement e t -> Node left mid right -- e already exists. returning input-tree
                           | otherwise -> if (e < mid) then (Node (insert e left) mid right) else (Node left mid (insert e right)) --add and return the updated tree
-- Exercise 5
createbinarysearchtree :: (Ord a, Eq a) => [a] -> BinaryTree a
createbinarysearchtree l =
  case l of
    [] ->  Leaf
    (h:t) -> insert h (createbinarysearchtree t)
        

-- Exercise 6 --TODO: do not convert to list 
remove :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
remove e t= 
  case t of
    Leaf -> Leaf
    Node left mid right | not(iselement e t) -> t
                        | otherwise -> createbinarysearchtree(removeif (\x -> e == x) (flatten t))
                            


----------------------------
-- Exercise Tower of Hanoi
----------------------------

type Rod = String
type Move = (Integer, Rod, Rod)
hanoi :: Integer -> Rod -> Rod -> Rod -> [Move]
hanoi n a b c | n == 0 = []
              | n == 1 = [(1,a,c)]
              | otherwise = hanoi (n-1) a c b ++ [(n,a,c)] ++ hanoi (n-1) b a c
{-
 n is the number of disks, which start from 1.}
  first, we move disks '1' to 'n-1' from a to b using rod c
  then move the biggest disk (n) from a to c
  shift disks 1 to n-1 from b to c using rod a

  -}
  