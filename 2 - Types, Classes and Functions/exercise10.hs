luhnDouble :: Int -> Int
luhnDouble x
    | doubled > 9 = doubled - 9
    | otherwise = doubled
    where doubled = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d
    | total `mod` 10 == 0 = True
    | otherwise = False
    where total = (luhnDouble a) + b + (luhnDouble c) + d