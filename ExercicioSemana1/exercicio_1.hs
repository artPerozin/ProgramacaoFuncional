isValidTriangle :: Int -> Int -> Int -> Bool
isValidTriangle a b c = a + b > c && a + c > b && b + c > a
