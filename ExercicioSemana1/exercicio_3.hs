triangleClass :: Int -> Int -> Int -> String
triangleClass a b c
    | a + b < c || a + c < b || b + c < a = "Triangulo Invalido"
    | a == b && b == c = "Equilatero"
    | a == b || b == c || a == c = "Isosceles"
    | otherwise = "Escaleno"
