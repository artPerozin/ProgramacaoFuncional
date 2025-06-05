todos :: [Bool] -> Bool
todos (x:xs) = if x == True then todos xs else False
todos [] = True
