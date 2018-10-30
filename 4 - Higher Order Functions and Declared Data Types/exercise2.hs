dec2Int :: [Int] -> Int
dec2Int xs = foldl (\ a b -> 10*a+b) 0 xs