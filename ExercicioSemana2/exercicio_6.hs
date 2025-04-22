pot2 :: Int -> [Int]
pot2 n = [2^x | x <- [1..n]]

-- lista que recebe um int n e retorna uma lista
-- com 2^x tal que x pertence à lista de numeros de 1 à n