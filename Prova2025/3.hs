replicar :: Int -> [a] -> [a]
replicar 0 _ = []
replicar _ [] = []
replicar n (x:xs) = replicarAux n x ++ replicar n xs
    where
        replicarAux :: Int -> a -> [a]
        replicarAux 0 _ = []
        replicarAux k y = y : replicarAux (k-1) y