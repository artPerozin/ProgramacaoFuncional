removePares :: [Int] -> [Int]
removePares x = [y | y <- x, y `mod` 2 /= 0 ]