soma2 :: [Int] -> [Int] -> [Int]
soma2 [] _ = []
--condição de parada, se a lista xs for vazia (sem cauda), parar recursividade
soma2 _ [] = []
--condição de parada para lista ys
soma2 (x:xs) (y:ys) = (x + y) : soma2 xs ys
--soma a cabeça das duas listas e chama função recursivamente a partir da cauda