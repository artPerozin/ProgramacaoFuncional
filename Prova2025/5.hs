menores [] = []
menores (x:xs) = if fst x < snd x then x : menores xs else menores xs