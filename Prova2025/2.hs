ultimoElemto :: [a] -> a
ultimoElemto [x] = x
ultimoElemto (_:xs) = ultimoElemto xs