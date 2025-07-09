semVogal :: [String] -> [String]
semVogal [] = []
semVogal (x:xs) = filterVogal x : semVogal xs

filterVogal :: String -> String
filterVogal [] = []
filterVogal (x:xs)
    |x == 'a' = filterVogal xs 
    |x == 'e' = filterVogal xs
    |x == 'i' = filterVogal xs
    |x == 'o' = filterVogal xs
    |x == 'u' = filterVogal xs
    |otherwise = [x] ++ filterVogal xs