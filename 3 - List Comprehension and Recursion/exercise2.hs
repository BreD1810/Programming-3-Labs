grid :: Int -> Int -> [(Int, Int)]
grid x y = [ (a, b) | a <- [0..x], b <- [0..y] ]

square :: Int -> [(Int, Int)]
square x = grid x x