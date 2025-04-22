--maneira 1:
nUltimos :: Int -> [a] -> [a]
nUltimos 0 _ = []
nUltimos _ [] = []
nUltimos n (x:xs) = nUltimos n xs

--maneira 2:
-- nUltimos :: Int -> [a] -> [a]
-- nUltimos n xs = drop (length xs - n) xs

--instancia a função a qual recebe um int e uma lista de a
--após isso, dá um drop do comprimento da lista - n  até a sua cauda e retorna