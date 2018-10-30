-- p - predicate determining the termination condition
-- h - describes how to insert the next element into the list
-- t - describes how the remainder should be processed.
unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x 
    | p x = []
    | otherwise = h x : unfold p h t (t x)


int2Bin :: Int -> [Int]
int2Bin = reverse . unfold (\a -> a <= 0) (\a -> a `mod` 2) (\a -> a `div` 2)


chop :: String -> Int -> [String]
chop [] _ = []
chop s n = (take n s) : (chop (drop n s) n)


map' :: (a -> b) -> [a] -> [b]
map' f [x] = [f x]
map' f  (x:xs) = [f(x)] ++ map' f xs


iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)