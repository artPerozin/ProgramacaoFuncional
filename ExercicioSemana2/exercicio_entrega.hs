pertence :: Eq a => a -> [a] -> Bool
pertence = elem

intercessao :: Eq a => [a] -> [a] -> [a]
intercessao xs ys = [x | x <- xs, x `elem` ys] 

inverso :: Eq a => [a] -> [a]
inverso = reverse

nUltimos :: Int -> [a] -> [a]
nUltimos n xs = drop (length xs - n) xs

soma2 :: [Int] -> [Int] -> [Int]
soma2 [] _ = []
soma2 _ [] = []
soma2 (x:xs) (y:ys) = (x + y) : soma2 xs ys

pot2 :: Int -> [Int]
pot2 n = [2^x | x <- [1..n]]

intercalacao :: [Int] -> [Int] -> [Int]
intercalacao [] ys = ys
intercalacao xs [] = xs
intercalacao (x:xs) (y:ys)
    | x <= y    = x : intercalacao xs (y:ys)
    | otherwise = y : intercalacao (x:xs) ys

menor :: (Ord a) => [a] -> a
menor [x] = x
menor (x:xs) = min x (menor xs) 

removerElem :: Eq a => a -> [a] -> [a]
removerElem _ [] = []
removerElem x (y:ys)
    | x == y    = ys
    | otherwise = y : removerElem x ys

ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar (x:xs) = ordenar [y | y <- xs, y < x] ++ [x] ++ ordenar [y | y <- xs, y > x]

ins :: Ord a => a -> [a] -> [a]
ins x [] = [x]
ins x (y:ys)
    | x < y     = x : y : ys
    | x == y    = y : ys
    | otherwise = y : ins x ys

enesimo :: Int -> [a] -> a
enesimo n xs = xs !! (n - 1)

repetir :: Int -> a -> [a]
repetir n e = replicate n e

numString :: Int -> String
numString n = show n

stringParaInt :: String -> Int
stringParaInt = read

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

import Data.Char (toLower)

minusculas :: String -> String
minusculas = map toLower