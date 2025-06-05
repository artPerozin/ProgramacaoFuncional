nprimeiros :: Int -> [a] -> [a]
nprimeiros 0 _ = []
nprimeiros _ [] = []
nprimeiros n (x:xs) = x : nprimeiros (n-1) xs