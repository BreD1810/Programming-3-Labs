all' :: (a -> Bool) -> [a] -> Bool
all' f xs = foldr (&&) True (map f xs)

any' :: (a -> Bool) -> [a] -> Bool
any' f xs = foldr (||) False (map f xs)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f xs = 
    let
        taker a bs
            | f a = a : bs
            | otherwise = []
    in foldr taker [] xs

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f xs =
    let
        taker a ([]:bss) = [a]:bss
        taker a ((b:bs):bss)
            | f b = (a:b:bs):bss
            | otherwise = [a]:(b:bs):bss
        (r:rs) = foldr taker [[]] xs
    in foldr (++) [] rs