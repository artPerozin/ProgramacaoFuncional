myMap:: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x: map f xs