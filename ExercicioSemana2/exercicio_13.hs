repetir :: Int -> a -> [a]
repetir 0 _ = []
-- maneira 1:
repetir n e = e : repetir (n - 1) e

-- maneira 2:
-- repetir n e = replicate n e