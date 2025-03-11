triangleClass :: Int -> Int -> Int -> String
triangleClass a b c
    | a == b && b == c = "Equilatero"
    | a == b || b == c || a == c = "Isosceles"
    | otherwise = "Escaleno"
