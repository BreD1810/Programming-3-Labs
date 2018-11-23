--COMP2209 Autumn 2017
--Sample solutions to Exercise Sheet 2

import Data.Char

-- Exercise One

--['a','b','c'] :: [Char]

--('a','b','c') :: (Char, Char, Char)

-- ['a',3,True] is not well-typed. Must have single type of elements in lists.

-- ('a',3,True) :: Num b => (Char, b, Bool).  Writing Int instead of Num b => etc is okay.

-- [ (False, '0'), (True,'1')] :: [(Bool, Char)]

-- ( [True,False] , ['0','1'] ) :: ([Bool], [Char])

-- [tail, init, reverse] :: [[a] -> [a]]

-- [] :: [a]

-- 2 : 3 : [] : 4 : 5 : [] :: Num [a] => [[a]].  
-- Looks like it should be ill-typed. But Num [a] will never hold so it practically is.

-- [] : [] :: [[a]]

-- Exercise Two

bools :: [Bool]
bools = [True,False]

nums :: [[Int]]
nums = [[1,2] , [4], []]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a,a)
copy x = (x,x)

apply :: (a -> b) -> a -> b
apply f x = f x

explode :: String -> [Char]
explode x = x
-- A String is already a 

-- Exercise Three
second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a, b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (t -> t) -> t -> t
twice f x = f ( f x )

-- Exercise Four
-- Function types are not structural in the same way that Lists and Tuples
-- are.  The constructor for function types is lambda (\x -> e). To compare
-- two such terms for equality we would need to compare the bodies of functions.
-- This could be done syntactically but this would be of limited use. For example
--  add x y = x + y  and add' x y = y + x  would be considered to be unequal.

-- Exercise Five
halve :: [a] -> ([a], [a])
halve xs = (take n xs , drop n xs) 
 where n = length xs `div` 2

-- It is not so easy to define this using pattern matching as we need to pattern match
-- n/2 elements of the list where n changes dynamically with the length of the list.

-- Exercise Six
third :: [a] -> a
third n = head (tail (tail n))

third' :: [a] -> a
third' n = n!!2

third'' :: [a] -> a
third'' (_ : _ : n : _ ) = n

-- Exercise Seven
safetail :: [a] -> [a]
safetail ns = if null ns then [] else tail ns

safetail' :: [a] -> [a]
safetail' ns | null ns = []
             | otherwise = tail ns

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (n:ns) = ns

-- The pattern matching solution is perhaps most concise.

-- Exercise Eight

{- (||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False

(||) :: Bool -> Bool -> Bool
False || False = False 
_ || _ = True

(||) :: Bool -> Bool -> Bool
False || b = b 
True || _ = True 
-}

(||) :: Bool -> Bool -> Bool
b || c | b ==c = b 
       | otherwise = True

-- Exercise Nine
enc :: Int -> String -> String
enc n [] = []
enc n (c:cs) = chr ( (ord c) + n ) : enc n cs

encrypt :: Int -> String -> (String, String -> String)
encrypt n s = (enc n s, dec)
  where dec [] = []
        dec (c:cs) = chr ((ord c) - n) : dec cs

-- Exercise Ten
luhnDouble :: Int -> Int
luhnDouble n | m > 9 = m-9
             | otherwise = m 
  where m = n*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn n1 n2 n3 n4 = 
     (luhnDouble n1 + n2 + luhnDouble n3 + n4) `mod` 10 == 0

