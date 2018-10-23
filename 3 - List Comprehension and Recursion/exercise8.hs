sumdown :: Int -> Int -> Int
sumdown x y
    | x == y = 0
    | x < 0 = 0
    | otherwise = x + sumdown (x-1) y