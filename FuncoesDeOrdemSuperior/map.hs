map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x:map' f xs

-- map recebe uma função e uma lista
-- aplica a função f para cada elem de uma lista x


