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

-- Exercise 3
data Expr = Val Int | Add Expr Expr | Sub Expr Expr

foldExpr :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> Expr -> b
foldExpr valFunc addFunc subFunc (Val x) = valFunc x
foldExpr valFunc addFunc subFunc (Add e1 e2) = addFunc (foldExpr valFunc addFunc subFunc e1) (foldExpr valFunc addFunc subFunc e2)
foldExpr valFunc addFunc subFunc (Sub e1 e2) = subFunc (foldExpr valFunc addFunc subFunc e1) (foldExpr valFunc addFunc subFunc e2)

eval :: Expr -> Int
eval = foldExpr (\x -> x) (\x y -> x + y) (\x y -> x - y) 

size :: Expr -> Int
size = foldExpr (\x -> 1) (\x y -> x + y + 1) (\x y -> x + y + 1)

testExpr = Val 1
testExpr2 = Add (Val 1) (Val 2)
testExpr3 = Sub (Val 2) (Val 1)
testExpr4 = Add (Val 1) (Sub (Val 2) (Val 1))