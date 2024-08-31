mapPares :: (a -> b -> c) -> [(a,b)] -> [c]
mapPares f = map (uncurry f)

armarPares :: [a] -> [b] -> [(a,b)]
armarPares _ [] = []
armarPares [] _ = []
armarPares (x:xs) (y:ys) = (x,y) : armarPares xs ys

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f [] _ = []
mapDoble f (x:xs) (y:ys) = f x y : mapDoble f xs ys