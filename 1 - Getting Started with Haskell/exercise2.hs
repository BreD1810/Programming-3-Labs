sum [] = 0
sum (x:xs) = x + sum xs

-- If you have a list containing just one element, x, then the above definition will return x.
-- xs in this case will be the empty set, as there is only one element.
-- sum [x] = x + sum [] == x + 0 = x
 