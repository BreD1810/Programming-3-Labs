euclid :: Int -> Int -> Int
euclid x y
    | x < 0 = error "First number is negative"
    | y < 0 = error "Second number is negative"
    | x == y = x
    | x > y = euclid (x-y) y
    | otherwise = euclid x (y-x)