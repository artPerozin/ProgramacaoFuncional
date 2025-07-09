myFold :: (a -> b -> b) -> b -> [a] -> b
myFold _ acc [] = acc
myFold f acc (x:xs) = f x (myFold f acc xs)
