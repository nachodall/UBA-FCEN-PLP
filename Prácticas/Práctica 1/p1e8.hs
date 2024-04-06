import Distribution.Simple.Utils (xargs)
uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f (x,y) = f x y

--a)
mapPares :: (a -> b -> c) -> [(a,b)] -> [c]
mapPares f = map (uncurry f) -- la descurrificamos para que nos deje pasarle un par (a,b) a la funcion que toma argumentos a->b

--b)
armarPares :: [a] -> [b] -> [(a,b)]
armarPares [] _ = []
armarPares _ [] = []
armarPares (x:xs) (y:ys) = (x,y) : armarPares xs ys 

--c)
mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble _ [] _ = [] 
mapDoble _ _ [] = []
mapDoble f (x:xs) (y:ys) = f x y : mapDoble f xs ys 

