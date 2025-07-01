import Data.List (sortBy, groupBy)
import Data.Function (on)

type Doc = String
type Line = String
type Word' = String

-- b) Numerar as linhas do documento
numLines :: Doc -> [(Int, Line)]
numLines doc = zip [1..] (lines doc)

-- c) Associar a cada ocorrência de uma palavra do documento
-- o número da linha em que essa palavra ocorre

allNumWords :: [(Int, Line)] -> [(Int, Word')]
allNumWords [] = []
allNumWords ((n,l):xs) = map (\w -> (n, w)) (words l) ++ allNumWords xs

-- d) Ordenar alfabeticamente as ocorrências de palavras no texto

sortLs :: [(Int, Word')] -> [(Int, Word')]
sortLs = sortBy (\(_, w1) (_, w2) -> compare w1 w2)

-- e) Juntar as várias ocorrências de cada palavra, produzindo, para cada palavra, a lista dos
-- números das linhas em que a palavra ocorre:

almalgamate :: [(Int, Word')] -> [([Int], Word')]
almalgamate xs = map (\ws -> (map fst ws, snd (head ws))) (groupBy (\x y -> snd x == snd y) (sortLs xs))

-- f) Eliminar, da lista de números de linhas em que cada palavra ocorre, as repetições de um
-- mesmo número de linha:

shorten :: [([Int],Word')] -> [([Int],Word')]
shorten = map (\(ns, w) -> (unique ns, w))

unique = foldr (\x seen -> if x `elem` seen then seen else x : seen) []

makeindex :: Doc -> [([Int], Word')]
makeindex txt = shorten . almalgamate . sortLs . allNumWords . numLines $ txt

formatIndex :: [([Int], Word')] -> String
formatIndex xs = unlines [w ++ " - " ++ show ns | (ns, w) <- xs]

main :: IO ()
main = do
    putStr "Arquivos: "
    arq <- getLine
    txt <- readFile arq
    putStr $ formatIndex (makeindex txt)
