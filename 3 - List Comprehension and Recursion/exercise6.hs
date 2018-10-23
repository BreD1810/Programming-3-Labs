find :: Eq a => a -> [ (a, b) ] -> [b]
find k t = [ v | (k', v) <- t, k==k' ]

-- The old definition using list comprehension
positionsOld :: Eq a => a -> [a] -> [Int]
positionsOld x xs = [ index | (x', index) <- zip xs [0..], x==x' ]

-- The new definition without using list comprehension
positions :: Eq a => a -> [a] -> [Int]
positions a xs = find a (zip xs [0..])