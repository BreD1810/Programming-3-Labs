-- Recursive natural number type
-- 0 = Zero
-- 1 = Succ Zero
-- 2 = Succ (Succ Zero)
data Nat = Zero | Succ Nat deriving (Eq, Ord, Show, Read)

even', odd' :: Nat -> Bool
even' Zero = True
even' (Succ n) = odd' n
odd' Zero = False
odd' (Succ n) = even' n

add :: Nat -> Nat -> Nat
add Zero x = x
add (Succ x) y = add x (Succ y) 

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ x) y = add y (mult x y)