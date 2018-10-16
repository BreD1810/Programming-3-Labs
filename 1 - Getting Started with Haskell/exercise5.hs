-- Quicksort, modified to match the exercise spec
quicksort [] = []
quicksort (x:xs) = quicksort ls ++ [x] ++ quicksort rs
    where
        ls = [ a | a <- xs , a < x ]
        rs = [ a | a <- xs , a > x ]

-- With this modified version, if you have a duplicated element, it will be lost.