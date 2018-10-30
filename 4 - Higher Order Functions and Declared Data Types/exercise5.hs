altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f f' [] = []
altMap f f' [x] = [f x]
altMap f f' (x:xs) = (f x) : altMap f' f xs 