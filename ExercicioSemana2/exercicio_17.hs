int2bin :: Int -> String
int2bin 0 = "0"
int2bin n
    | n > 0     = reverse (helper n)
    | otherwise = error "Negative numbers are not supported"
    where
        helper 0 = ""
        helper x = let (q, r) = x `divMod` 2 in show r ++ helper q