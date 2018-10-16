import Data.Char

myEncryption :: Int -> String -> (String, String -> String)
myEncryption n xs = (enc n xs, enc (-n))
    where 
        enc n [] = []
        enc n (x:xs) = [chr(n + ord x)] ++ enc n xs