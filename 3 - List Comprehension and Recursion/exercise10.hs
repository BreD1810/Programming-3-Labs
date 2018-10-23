merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x < y = [x] ++ merge xs (y:ys)
    | otherwise = [y] ++ merge (x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
    let 
        halve ys = (take halfLength ys, drop halfLength ys)
            where halfLength = length ys `div` 2
        (left, right) = halve xs
    in merge (mergeSort left) (mergeSort right)