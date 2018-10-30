luhnDouble :: Int -> Int
luhnDouble x
    | doubled > 9 = doubled - 9
    | otherwise = doubled
    where doubled = x * 2

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f f' (x:xs) = (f x) : altMap f' f xs 

luhn :: [Int] -> Bool
luhn xs
    | luhnTotal xs `mod` 10 == 0 = True
    | otherwise = False


luhnTotal :: [Int] -> Int
luhnTotal xs
    | (even.length) xs = (sum . altMap luhnDouble (\x -> x)) xs
    | otherwise = (sum . altMap (\x -> x) luhnDouble) xs