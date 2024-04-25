
module Practicum1 where

{-
Name:           <Amin Soleimani>
VU-net id:      <asi273>
Student number: <Student number>
Discussed with: <In case you discussed the exercises with someone else,
                 please mention his/her name(s) explicitly here>
Remarks:        <In case something need special attention,
                 please tell us>
Sources:        <in case you used sources such as books or webpages
                 please mention them here>
-}

-- Below you will find templates for the exercises. For each exercise,
-- replace 'undefined' by your definition and supply at least two different
-- meaningful tests to show that your code produces sane results. The
-- tests may be given in comments (see exercise 1).

-- Exercise 1
maxi :: Integer -> Integer -> Integer
maxi a b  = if b < a then a else b

-- maxi 2 3 == 3
-- maxi 3 2 == 2

-- Exercise 2
fourAscending :: Integer -> Integer -> Integer -> Integer -> Bool
fourAscending a b c d = a < b && b < c && c < d

-- fourAscending (-1) 0 1 2 == True
-- fourAscending 0 1 2 4 == True
-- fourAscending 0 1 2 2 == False

-- Exercise 3
fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual a b c d = a == b && b == c && c == d

-- fourEqual 1 2 1 1 == False
-- fourEqual 1 1 1 1 == True

-- Exercise 4
fourDifferent :: Integer -> Integer -> Integer -> Integer -> Bool
fourDifferent a b c d = (a /= b && a /= c && a /= d) && (b /= c && b /= d) && (c /= d)

-- fourDifferent 1 2 1 2 == False
-- fourDifferent 1 2 3 0 == True

-- Exercise 5
{-
threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent a b c = ( ( a /= b) && (b /= c) )

We are not checking if a and c are different or not. so when a == c and  a /= b and b /= c, function fails.
example input: threeDifferent 1 2 1
-}

-- Exercise 6
factorial :: Integer -> Integer
factorial n | (n == 0) = 1
            | (n > 0) = n * factorial(n - 1)

-- factorial 4 == 24
-- factorial 5 == 120

-- Exercise 7
fib :: Integer -> Integer
fib n | (n == 0) = 0
      | (n == 1) = 1
      | (n > 1) = fib(n - 1) + fib(n - 2)

-- fib 0 == 0
-- fib 10 == 55


-- Exercise 8
-- it is possible to define auxiliary functions
strangeSummation :: Integer -> Integer

--strangeSummation n = sum [n..n+7]

-- strangeSummation n = foldr (+) 0 [n..n+7]

strangeSummation n = sumList (range n (n+7))


-- strangeSummation 10 == 108
-- strangeSummation 1 == 36

-- Exercise 9
lengthList :: [Integer] -> Integer
lengthList []  = 0
lengthList (h:t) = 1 + lengthList t

lengthListAlternative :: [Integer] -> Integer
lengthListAlternative l =
  case l of
    [] -> 0
    (h:t) -> 1 + (lengthListAlternative t)

sumList :: [Integer] -> Integer
sumList n = 
  case n of
    [] -> 0
    (h:t) -> h + (sumList t) 

-- sumList [1,2,3,4] == 10
-- sumList [] == 0

-- Exercise 10
doubleList :: [Integer] -> [Integer]
doubleList n =
  case n of
    [] -> []
    (h:t) -> (h * 2) : doubleList t

-- doubleList [1,2,3] == [2,4,6]
-- doubleList [] == []

-- Exercise 11
myappend :: [a] -> [a] -> [a]
myappend a b =
  case a of
    [] -> b 
    (h:t) -> h : myappend t b

-- myappend [] [1,2,3] == [1,2,3]
-- myappend [1,2,3] [] == [1,2,3]
-- myappend [1,2,3] [4,5,6] == [1,2,3,4,5,6]

-- Exercise 12
myreverse :: [a] -> [a]
myreverse n =
  case n of
    [] -> []
    (h:t) -> myappend (myreverse t) [h]  -- append the head in front of the [tail]

-- myreverse [1,2,2,3] == [3,2,2,1]
-- myreverse [] == []

-- Exercise 13
mymember :: (Eq a) => a -> [a] -> Bool
mymember n l = 
  case l of
    [] -> False
    (h:t) -> h == n || mymember n t

-- mymember 1 [] == False
-- mymember 20 (range 1 100) == True
-- mymember 'x' ['a'..'z'] == True

-- Exercise 14
mysquaresum :: [Integer] -> Integer
mysquaresum n =
  case n of
    [] -> 0
    (h:t) -> (h * h) + mysquaresum t
-- mysquaresum [] == 0
-- mysquaresum [1,2,3] == 14

-- Exercise 15
range :: Integer -> Integer -> [Integer]
-- range a b = if a <= b then a : range (a + 1) b else []
range a b 
  | a == b = [a]
  | a > b = []
  | otherwise = a : range (a + 1) b

-- range 1 1 == [1]
-- range 4 1 == []
-- range 1 5 == [1,2,3,4,5]

-- Exercise 16
myconcat :: [[a]] -> [a]
myconcat l =
  case l of
    [] -> []
    (h:t) -> myappend h (myconcat t)

-- myconcat [[1,2,3,4], [], [1,2,3], [1,1,1],[]] == [1,2,3,4,1,2,3,1,1,1]
-- myconcat [[],[]] == []

-- Exercise 17
insert :: Ord a => a -> [a] -> [a] --TODO: replace 
insert n l 
  | l == [] = [n]
  | n <= head l = n:l
  | otherwise = (head l) : (insert n (tail l))

{-
insert n l =
  case l of
    [] -> n:[]
    (h:t) -> if (n < h) then n:h:t else h : insert n t
-}
-- insert 1 [] == [1]
-- insert 3 [1,2,3,4,5] == [1,2,3,3,4,5]
-- insert 4 [1,2,3] == [1,2,3,4]

insertionsort :: Ord a => [a] -> [a]
insertionsort l =
  case l of
    [] -> []
    (h:t) ->  insert h (insertionsort t)

{-
insertionsort l 
  | l == [] = []
  | otherwise = insert (head l) (insertionsort (tail l))
-}

-- insertionsort [] == []
-- insertionsort [4,3,2,1,1] == [1,1,2,3,4]
-- insertionsort [5,2,1,6,8,4,3,1,0,0,4,2,3,44,1] == [0,0,1,1,1,2,2,3,3,4,4,5,6,8,44]


-- Exercise 18

minim :: Ord a => [a] -> a
minim l =
  case l of 
    [x] -> x 
    (h:m:t) -> minim ((if h <= m then h else m):t)
    
    

-- Exercise 19

-- TODO: ask
-- Not sure if I'm supposed to add examples of using filter to this exercise or not. 
divisable :: Integer -> Integer -> Bool
divisable x y = (y `mod` x == 0 )
divisableInRange :: Integer -> Integer -> Integer -> [Integer]
divisableInRange n a b
  | n > b = []
  | otherwise = filter (divisable n ) (range a b)

-- divisableInRange 7 1 50 ==  [7,14,21,28,35,42,49]
-- divisableInRange 12 1 10 == []

removeSpace :: [Char] -> [Char]
removeSpace str = filter (\x -> x /= ' ') str

-- removeSpace "Hello World!  " == "HelloWorld!"
-- removeSpace [  ] == []

smallerList :: Ord a => a -> [a] -> [a]
smallerList n l 
  | l == [] = []
  | otherwise = filter (\x -> x <= n ) l

-- smallerList 4 [1,2,3,4,5,6,7,8,9] == [1,2,3,4]
-- smallerList 2 [-1,-2,-3,1,2,3,4] == [-1,-2,-3,1,2]

largerList :: Ord a => a -> [a] -> [a]
largerList n l 
  | l == [] = []
  | otherwise = filter (\x -> x > n ) l

quicksort :: Ord a => [a] -> [a]
quicksort l = 
  case l of
    [] -> []
    (h:t) -> myappend (quicksort (smallerList h t)) (h:quicksort (largerList h t))

-- quicksort [9,8,7,6,5,4,3,2,1] == [1,2,3,4,5,6,7,8,9]
-- quicksort [] == []
-- quicksort "hello world!" ==  " !dehllloorw"

-- Exercise 20
evensB :: [Integer] -> [Integer]
evensB l = [ x | x <- l , x `mod` 2 == 0]

-- evensB [1,3,5,6] == [6]
-- evensB [] == []

-- Exercise 22 TODO: add examples for map + non recursive
mymap :: (a -> b) -> [a] -> [b]
mymap f l =
  case l of
    [] -> []
    (h:t) -> f h : mymap f t

-- mymap (\x -> x + 1) [1,2,3] == [2,3,4]
-- mymap removeSpace [" f  i  r   s  t  " , "s e c o n d  "] == ["first","second"]

-- Exercise 23
twice :: (a -> a) -> a -> a
twice f x = f (f x)
-- it is also possible to use . operator: (f . f) x
-- twice (\x -> x + 1) 0 == 2
-- twice (\x -> x/2) 10 == 2.5

-- Exercise 24
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x= (f . g) x

-- compose (\x -> x + 4) (\x -> x * 5) 7 == 39
-- compose (\x -> x * 10) (\x -> x / 10) 2 == 2

-- Exercise 25
mylast :: [a] -> a
mylast l =  head (myreverse l)

-- mylast "abcdef" == 'f'
-- mylast [1,2,3,4] == 4


-- Exercise 26
mylastb :: [a] -> a
mylastb l = if (length l == 1) then (head l) else head(drop((length l) - 1) l)

-- mylast "abcdef" == 'f'
-- mylast [1,2,3,4] == 4

-- Exercise 27
myinit, myinitb :: [a] -> [a]
myinit l = 
  case l of
    [] -> []
    (h:t) -> if (length l < 2) then [] else h:myinit (tail(l))
myinitb l = take (length l - 1) l

--myinit [1,2,3] == [1,2]
--myinit [1] == []
--myinit [1,2,3] == myinitb [1,2,3]


-- Exercise 28
mysecondconcat :: [[a]] -> [a]
mysecondconcat l = 
  case l of
    [] -> []
    (h:t) -> foldr (++) [] (h:t)
--mysecondconcat [[1,2,3,4], [], [1,2,3], [1,1,1],[]] == [1,2,3,4,1,2,3,1,1,1]

mysecondreverse :: [a] -> [a]
mysecondreverse l = 
  case l of
    [] -> []
    (h:t) -> foldr (\x -> \y -> y ++ [x]) [] (h:t)
-- mysecondreverse [1,2,2,3] == [3,2,2,1]
-- mysecondreverse [] == []

-- Exercise 28
mythirdconcat :: [[a]] -> [a]
mythirdconcat l= 
  case l of
    [] -> []
    (h:t) -> foldl (++) [] (h:t)
--mythirdconcat [[1,2,3,4], [], [1,2,3], [1,1,1],[]] == [1,2,3,4,1,2,3,1,1,1]

mythirdreverse :: [a] -> [a]
mythirdreverse l = 
  case l of
    [] -> []
    (h:t) -> foldl (\x -> \y -> [y] ++ x) [] l
-- mythirdreverse [1,2,2,3] == [3,2,2,1]
-- mythirdreverse [] == []


-- Exercise 29

prefix :: [a] -> [[a]]
prefix l =
  case l of 
    [] -> [[]]
    (h:t) -> prefix (myinit (h:t)) ++ [l]
--prefix [1,2,3] == [[],[1],[1,2],[1,2,3]]

-- TODO: check why this does not work
{-
prefix l
  | (l == []) = [[]]
  | otherwise= l : prefix (myinit (tail l))

-}

myfourthconcat :: [[a]] -> [a]
myfourthconcat l = 
  case l of
    [] -> []
    (h:t) -> foldr myappend [] l 



myfourthreverse :: [a] -> [a]
myfourthreverse l =
  case l of
    [] -> []
    (h:t) -> foldr (:) [h] (myfourthreverse t)
