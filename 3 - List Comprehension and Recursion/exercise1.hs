-- Sum of the squares of odd numbers and cubes of even numbers for the first 100 ints
sum [ x^2 | x <- [0, 2..100] ] + sum [y^3 | y <- [1, 3..100]]