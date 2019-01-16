-- Exercise 1
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs a (Leaf b) = a == b
occurs a (Node left b right) | result == EQ = True
                             | result == LT = occurs a left
                             | otherwise = occurs a right
    where
        result = compare a b

testTree = Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Leaf 5)