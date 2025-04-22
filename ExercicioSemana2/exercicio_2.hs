-- maneira 1
pertence :: Eq a => a -> [a] -> Bool
pertence _ [] = False
pertence x (y:ys)
    | x == y    = True
    | otherwise = pertence x ys

intercessao :: Eq a => [a] -> [a] -> [a]
intercessao xs ys = [x | x <- xs, pertence x ys]

-- maneira 2:

-- intercessao :: Eq a => [a] -> [a] -> [a]
-- intercessao xs ys = [x | x <- xs, x `elem` ys]