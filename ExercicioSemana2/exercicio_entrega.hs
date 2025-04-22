import Data.Char (ord, isDigit)

-- maneira 1

pertence :: Eq a => a -> [a] -> Bool
pertence _ [] = False
pertence x (y:ys)
    | x == y    = True
    | otherwise = pertence x ys

-- maneira 2:

-- pertence :: Eq a => a -> [a] -> Bool
-- pertence = elem

intercessao :: Eq a => [a] -> [a] -> [a]
intercessao xs ys = [x | x <- xs, pertence x ys]

-- maneira 2:

-- intercessao :: Eq a => [a] -> [a] -> [a]
-- intercessao xs ys = [x | x <- xs, x `elem` ys]

--maneira 1:

inverso :: Eq a => [a] -> [a]
inverso [] = []
inverso (x:xs) = inverso xs ++ [x]

--maneira 2:

-- inverso :: Eq a => [a] -> [a]
-- inverso = reverse

-- maneira 1:
-- nPrimeiros :: Int -> [a] -> [a]
-- nPrimeiros _ [] = []
-- nPrimeiros 0 _ = []
-- nPrimeiros n (x:xs) = x : nPrimeiros (n-1) xs

-- nUltimos :: Int -> [a] -> [a]
-- nUltimos 0 _ = []
-- nUltimos _ [] = []
-- nUltimos n xs = inverso (nPrimeiros (n) (inverso xs))

-- maneira 2:
nUltimos :: Int -> [a] -> [a]
nUltimos n xs = drop (length xs - n) xs

-- instancia a função a qual recebe um int e uma lista de a
-- após isso, dá um drop do comprimento da lista - n  até a sua cauda e retorna

soma2 :: [Int] -> [Int] -> [Int]
soma2 [] _ = []
--condição de parada, se a lista xs for vazia (sem cauda), parar recursividade
soma2 _ [] = []
--condição de parada para lista ys
soma2 (x:xs) (y:ys) = (x + y) : soma2 xs ys
--soma a cabeça das duas listas e chama função recursivamente a partir da cauda

pot2 :: Int -> [Int]
pot2 n = [2^x | x <- [1..n]]

-- lista que recebe um int n e retorna uma lista
-- com 2^x tal que x pertence à lista de numeros de 1 à n

intercalacao :: [Int] -> [Int] -> [Int]
intercalacao [] ys = ys
intercalacao xs [] = xs
intercalacao (x:xs) (y:ys)
    | x <= y    = x : intercalacao xs (y:ys)
    | otherwise = y : intercalacao (x:xs) ys

menor :: (Ord a) => [a] -> a
menor [x] = x --se a lista tiver somente um elemento
menor (x:xs) = min x (menor xs) 
--compara a cabeça da lista com menor da cauda


removerElem :: Eq a => a -> [a] -> [a]
removerElem _ [] = [] --caso não haja elemente para remover, retorne a lista
removerElem x (y:ys)
    | x == y    = ys --se a cabeça coicidir com x, a lista continuará a partir da cauda
    | otherwise = y : removerElem x ys

ordenar :: Ord a => [a] -> [a] --os elementos devem ser ordenados
ordenar [] = [] --se a lista for vazia, a mesma será retornada
ordenar (x:xs) = ordenar [y | y <- xs, y < x] ++ [x] ++ ordenar [y | y <- xs, y > x]
--pega os números menores que o pivô
--concatena com o pivô
--pega os números maiores que o pivô

-- Define uma função 'ins' que insere um elemento 'x' em uma lista ordenada.
-- A restrição 'Ord a' indica que os elementos do tipo 'a' precisam ser comparáveis (ordenáveis).
ins :: Ord a => a -> [a] -> [a]

-- Caso base: se a lista for vazia, insere 'x' como único elemento.
ins x [] = [x]

-- Caso recursivo: se a lista não for vazia, faz pattern matching no primeiro elemento 'y' e no restante 'ys'
ins x (y:ys)
    -- Se 'x' for menor que 'y', insere 'x' antes de 'y' para manter a ordem
    | x < y     = x : y : ys
    
    -- Se 'x' for igual a 'y', ignora 'x' (não insere duplicatas) e retorna a lista original
    | x == y    = y : ys

    -- Caso contrário, mantém 'y' e continua a busca recursivamente no restante da lista 'ys'
    | otherwise = y : ins x ys

enesimo :: Int -> [a] -> a
enesimo n xs = xs !! (n - 1)
--pega o enesimo numero pelo indice da lista

repetir :: Int -> a -> [a]
repetir 0 _ = []

-- maneira 1:
repetir n e = e : repetir (n - 1) e

-- maneira 2:
-- repetir n e = replicate n e

-- maneira 1:
numString :: Int -> String
numString n
  | n < 0     = '-' : numString (-n)
  | n < 10    = [toChar n]
  | otherwise = numString (n `div` 10) ++ [toChar (n `mod` 10)]
  where
    toChar x = ['0'..'9'] !! x

-- maneira 2:
-- numString' :: Int -> String
-- numString n = show n


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

bin2int :: String -> Int
bin2int bin = foldl (\acc x -> acc * 2 + digitToInt x) 0 bin
    where
        digitToInt c = if c == '1' then 1 else 0

int2bin :: Int -> String
int2bin 0 = "0"
int2bin n
    | n > 0     = reverse (helper n)
    | otherwise = error "Negative numbers are not supported"
    where
        helper 0 = ""
        helper x = let (q, r) = x `divMod` 2 in show r ++ helper q

-- maneira 1:
minusculas :: String -> String
minusculas = map toLowerManual
  where
    toLowerManual c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise            = c

-- maneira 2:
-- minusculas = map toLower