remover :: Eq a => a -> [a] -> [a]
remover n (x:xs) = [y | y <- (x:xs), y /= n]