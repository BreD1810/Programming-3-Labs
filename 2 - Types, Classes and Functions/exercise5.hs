halve :: [a] -> ([a], [a])
halve xs 
    | length xs `mod` 2 == 1 = error "Not even length"
    | otherwise = (take halfLength xs, drop halfLength xs)
    where halfLength = length xs `div` 2

--This cannot be done with pattern matching,
--as you cannot determine if a list is odd in length