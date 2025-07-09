semVogal :: [String] -> [String]
semVogal [] = []
semVogal (x:xs) = filterVogal x : semVogal xs

filterVogal :: String -> String
filterVogal [] = []
filterVogal (x:xs)
    | x `elem` "aeiouAEIOU" = filterVogal xs
    |otherwise = [x] ++ filterVogal xs