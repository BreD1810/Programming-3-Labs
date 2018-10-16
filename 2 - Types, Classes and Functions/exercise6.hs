-- head and tail
third :: [a] -> a
third xs 
    | length xs < 3 = error "Less than 3 elements"
    | otherwise = head(tail(tail(xs)))

--list indexing
third' :: [a] -> a
third' xs
    | length xs < 3 = error "Less than 3 elements"
    | otherwise = xs !! 2

--pattern matching
third'' :: [a] -> a
third'' [] = error "Empty list"
third'' (_:[]) = error "Only 1 element"
third'' (_:_:[]) = error "Only 2 elements"
third'' (_:_:x:_) = x 