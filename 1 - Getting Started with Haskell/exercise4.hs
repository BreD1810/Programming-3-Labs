-- Quicksort, supplied in the exercise spec
quicksort [] = []
quicksort (x:xs) = quicksort ls ++ [x] ++ quicksort rs
    where
        ls = [ a | a <- xs , a <= x ]
        rs = [ a | a <- xs , a > x ]

-- Quicksort in descending order
quicktros [] = []
quicktros (x:xs) = quicktros ls ++ [x] ++ quicktros rs
    where
        ls = [ a | a <- xs , a > x ]
        rs = [ a | a <- xs , a <= x ]