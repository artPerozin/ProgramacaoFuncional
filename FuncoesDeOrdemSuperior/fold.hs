-- usar fold para implementar uma função all'
-- recebe uma lista de dados boleanos
-- retorna True se todos os elementos forem True
-- e False se algum elemento for False

--foldr 

all' :: [Bool] -> Bool
all' xs = foldr (\x acc -> x && acc) True xs

all'' :: [Bool] -> Bool
all'' (x:xs) = if x == False then False else all'' xs
all'' [] = True

