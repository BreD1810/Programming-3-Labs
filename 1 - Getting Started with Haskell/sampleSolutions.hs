--COMP2209 Autumn 2017
--Sample solutions to Exercise Sheet 1

-- Exercise One
-- double (double 2)
-- = double 2 + double 2
-- = 2 + 2 + double 2
-- = 2 + 2 + 2 + 2 
-- = 4 + 2 + 2
-- = 6 + 2
-- = 8
-- Yes, there are many ways of doing this. Resolving the innermost double 2 
-- would be more efficient than the above

-- Exercise Two
-- sum [ x ] 
-- = x + sum []
-- = x + 0 
-- = x

-- Exercise Three

product :: [Int] -> Int 
product [] = 1
product (x : xs) = x * product xs

-- Exercise Four

quicktros :: [Int] -> [Int]
quicktros [] = []
quicktros (x : xs) = quicktros rs ++ [x] ++ quicktros ls
    where  ls = [ a | a <- xs , a <= x ]
           rs = [ a | a <- xs , a > x ]

-- Exercise Five
quicksort' [] = []
quicksort' (x:xs) = quicksort' ls ++ [x] ++ quicksort' rs
                   where 
                     ls = [ a | a <- xs , a < x ]
                     rs = [ a | a <- xs , a > x ]

-- The effect of using < rather than <= in ls is that duplicate entries in the list would
-- be removed as part of the sorting process.

-- Exercise Six
(2^3)*4
(2*3)+(4*5)
2 + (3*(4^5))
(2^2)+(2^2)

-- Exercise Seven
-- Can't use uppercase N as a variable name
-- Need backquotes for div
-- Breaks the layout rule in the where clause - columns must be aligned
n = a `div` length xs
    where 
      a = 10
      xs = [1,2,3,4,5]

-- Exercise Eight
last1 :: [Int] -> Int
last1 xs = xs !! n
   where n = (length xs -1)

last2 :: [Int] -> Int
last2 xs = head ( drop n xs )
   where n = (length xs -1)

