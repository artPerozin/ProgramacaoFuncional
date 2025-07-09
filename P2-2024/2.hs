compDuplas :: [(a, b)] -> [(b, a)]
compDuplas [] = []
compDuplas = map (\(a, b) -> (b, a))

--metodos alternativos:

-- compDuplas (x:xs) = (snd x, fst x): compDuplas xs
-- compDuplas ((a, b):xs) = (b, a) : compDuplas xs
