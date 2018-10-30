altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f f' (x:xs) = (f x) : altMap f' f xs 