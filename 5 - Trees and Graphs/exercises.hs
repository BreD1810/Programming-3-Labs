testTree = Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Leaf 5)

-- Exercise 1
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs a (Leaf b) = a == b
occurs a (Node left b right) | result == EQ = True
                             | result == LT = occurs a left
                             | otherwise = occurs a right
    where
        result = compare a b

-- Exercise 2
foldTree :: (a -> b) -> (a -> b -> b -> b) -> Tree a -> b
foldTree leaff nodef (Leaf x) = leaff x
foldTree leaff nodef (Node left x right) = nodef x (foldTree leaff nodef left) (foldTree leaff nodef right)

flatten :: Tree a -> [a]
flatten = foldTree (\x -> [x]) (\x ls rs -> ls ++ (x:rs))