bools :: [Bool]
bools = [True, False, True, False]

nums :: [[Int]]
nums = [[1, 2, 3], [4, 5, 6]]

add :: Int -> Int -> Int -> Int
add a b c = a+b+c

copy :: a -> (a, a)
copy a = (a, a)

apply :: (a -> b) -> a -> b
apply x y = x(y)

explode :: String -> [Char]
explode [] = []
explode (x:xs) = [x] ++ explode xs