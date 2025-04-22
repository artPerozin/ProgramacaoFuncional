-- maneira 1:
-- nPrimeiros :: Int -> [a] -> [a]
-- nPrimeiros _ [] = []
-- nPrimeiros 0 _ = []
-- nPrimeiros n (x:xs) = x : nPrimeiros (n-1) xs

-- inverso :: [a] -> [a]
-- inverso [] = []
-- inverso (x:xs) = inverso xs ++ [x]

-- nUltimos :: Int -> [a] -> [a]
-- nUltimos 0 _ = []
-- nUltimos _ [] = []
-- nUltimos n xs = inverso (nPrimeiros (n) (inverso xs))

-- maneira 2:
-- nUltimos :: Int -> [a] -> [a]
-- nUltimos n xs = drop (length xs - n) xs

-- instancia a função a qual recebe um int e uma lista de a
-- após isso, dá um drop do comprimento da lista - n  até a sua cauda e retorna
