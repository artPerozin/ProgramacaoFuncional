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
