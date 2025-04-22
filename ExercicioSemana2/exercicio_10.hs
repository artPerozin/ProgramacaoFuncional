ordenar :: Ord a => [a] -> [a] --os elementos devem ser ordenados
ordenar [] = [] --se a lista for vazia, a mesma será retornada
ordenar (x:xs) = ordenar [y | y <- xs, y < x] ++ [x] ++ ordenar [y | y <- xs, y > x]
--pega os números menores que o pivô
--concatena com o pivô
--pega os números maiores que o pivô