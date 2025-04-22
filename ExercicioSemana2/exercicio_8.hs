menor :: (Ord a) => [a] -> a
menor [x] = x --se a lista tiver somente um elemento
menor (x:xs) = min x (menor xs) 
--compara a cabeça da lista com menor da cauda