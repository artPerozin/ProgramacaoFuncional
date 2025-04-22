import Data.Char (ord, isDigit)

stringParaInt :: String -> Int
stringParaInt [] = error "String vazia"
stringParaInt ('-':xs) = - (stringParaInt xs)
stringParaInt xs = foldl (\acc x -> acc * 10 + charToDigit x) 0 xs
  where
    charToDigit c
      | isDigit c = ord c - ord '0'
      | otherwise = error ("Caractere inválido: " ++ [c])

-- maneira 2:
-- stringParaInt :: String -> Int
-- stringParaInt = read
