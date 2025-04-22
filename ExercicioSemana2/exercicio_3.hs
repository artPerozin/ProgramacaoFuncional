--maneira 1:

inverso :: Eq a => [a] -> [a]
inverso [] = []
inverso (x:xs) = inverso xs ++ [x]

--maneira 2:

-- inverso :: Eq a => [a] -> [a]
-- inverso = reverse