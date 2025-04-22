bin2int :: String -> Int
bin2int bin = foldl (\acc x -> acc * 2 + digitToInt x) 0 bin
    where
        digitToInt c = if c == '1' then 1 else 0