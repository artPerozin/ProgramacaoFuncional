tam :: [a] -> Int

tam [] = 0
tam (x:xs) = 1 + tam xs
