data RInt = Zero | Succ RInt | Pred RInt deriving Show

normalise :: RInt -> RInt
normalise r =
    let
        toInt :: RInt -> Int
        toInt Zero = 0
        toInt (Succ x) = toInt x + 1
        toInt (Pred x) = toInt x - 1
        generateRInt :: Int -> RInt
        generateRInt 0 = Zero
        generateRInt x
            | x > 0 = Succ(generateRInt(x-1))
            | otherwise = Pred(generateRInt(x+1))
    in generateRInt(toInt r)

--odd' :: RInt -> Bool


--even' :: RInt -> Bool


--add :: RInt -> RInt -> RInt


--mult :: RInt -> RInt -> RInt
