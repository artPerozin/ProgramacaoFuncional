segundos :: [(a,b)] -> [b]
segundos [] = []
segundos (x:xs) = [y | (_, y) <- [x]] ++ segundos xs
