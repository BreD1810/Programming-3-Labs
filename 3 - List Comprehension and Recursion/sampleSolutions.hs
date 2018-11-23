--COMP2209 Autumn 2017
--Sample solutions to Exercise Sheet 3

-- Exercise One
sumsqevencubeodd = sum [ n^2 | n <- [0,2..100] ] + sum [ n^3 | n <- [1,3..99] ]

-- Exercise Two
grid :: Int -> Int -> [(Int , Int)]
grid m n = [ (x,y) | x <- [0..m] , y <-[0..n] ]

square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n , x /= y ]

-- Exercise Three
replicate :: Int -> a -> [a]
replicate n a = [ a | _ <- [1..n]]

-- Exercise Four
pyths :: Int -> [(Int,Int,Int)]
pyths n = [ (x,y,z) | x <- [1..n] , y <- [1..n], z <- [1..n] , x^2 + y^2 == z^2]


-- Exercise Five
perfect :: Int -> Bool
perfect n = factorSum == n
    where factorSum = sum [ m | m <- [1..n-1], n `mod` m == 0 ]

perfects :: Int -> [Int]
perfects m = [ n | n <- [1..m], perfect n]    

-- Exercise Six
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [ v | (k',v) <- t , k==k']

positions :: Eq a => a -> [a] ->[Int]
positions a as = find a (zip as [0..])

-- Exercise Seven
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [ x * y | (x,y) <- zip xs ys]

-- Exercise Eight
sumDown :: Int -> Int -> Int
sumDown n m | m == n = n  
sumDown n m | n > m = n + sumDown (n-1) m

-- Exercise Nine
euclid :: Int -> Int -> Int 
euclid n m | n==m = n 
           | n < m = euclid n (m-n)
           | otherwise = euclid (n-m) m

-- Exercise Ten
merge :: Ord a => [a] -> [a] -> [a]
merge [] bs = bs
merge as [] = as
merge (a:as) (b:bs) | a <= b = a : merge as (b:bs)
                    | otherwise = b : merge (a:as) bs


halve :: [a] -> ([a],[a])
halve ns = (take n ns , drop n ns) 
             where n = (length ns) `div` 2


mergesort :: Ord a => [a] -> [a]
meresort [] = []
mergesort [a] = [a]
mergesort as = merge (mergesort as1) (mergesort as2)
             where (as1,as2) = halve as


