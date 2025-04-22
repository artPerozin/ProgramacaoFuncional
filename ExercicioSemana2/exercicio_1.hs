-- maneira 1

pertence :: Eq a => a -> [a] -> Bool
pertence _ [] = False
pertence x (y:ys)
    | x == y    = True
    | otherwise = pertence x ys

-- maneira 2:

-- pertence :: Eq a => a -> [a] -> Bool
-- pertence = elem