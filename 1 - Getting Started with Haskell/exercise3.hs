-- Return the product of a list of elements
product' [] = 1
product' (x:xs) = x * product' xs