--if-then-else
safetail :: [a] -> [a]
safetail xs =
    if length xs == 0 then []
    else tail xs

--guarded equations
safetail' :: [a] -> [a]
safetail' xs
    | null xs == True = []
    | otherwise = tail xs

--pattern matching
safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs