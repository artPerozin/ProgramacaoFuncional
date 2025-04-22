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