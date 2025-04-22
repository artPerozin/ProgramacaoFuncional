
removerElem :: Eq a => a -> [a] -> [a]
removerElem _ [] = [] --caso não haja elemente para remover, retorne a lista
removerElem x (y:ys)
    | x == y    = ys --se a cabeça coicidir com x, a lista continuará a partir da cauda
    | otherwise = y : removerElem x ys