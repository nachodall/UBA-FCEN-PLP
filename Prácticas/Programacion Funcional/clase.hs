(.) :: (a -> b) -> (c -> a) -> c -> b
(.) f g x = f (g x) 

flipp :: (a -> b -> c) -> b -> a -> c 
flipp f x y = f y x 

($) :: (a -> b) -> a -> b 
($) f = f 

constt :: a -> b -> a 
constt x y = x 

maximo :: Ord a => [a] -> a 
maximo = foldr1 (\x max -> if x > max then x else max) 

listaMasCorta :: [[a]] -> [a]
listaMasCorta = foldr1 (\x max -> if length x < length max then x else max)

filterConFold :: (a -> Bool) -> [a] -> [a]
filterConFold pred = foldr(\x rs -> if pred x then x:rs else rs) []

soloPuntosFijos :: Int -> [Int->Int] -> [Int->Int]
soloPuntosFijos n = filterConFold (\f -> f n == n) 

mapConFold :: (a -> b) -> [a] -> [b]
mapConFold f = foldr (\x rs -> f x : rs) [] 

reverseAnidado :: [[Char]] -> [[Char]]
reverseAnidado xs =  reverse (map reverse xs) 

paresCuadrados :: [Int] -> [Int] 
paresCuadrados = map (\x -> if even x then x^2 else x) 

listaComp :: (b -> a) -> [b] -> (b -> Bool) -> [a]
listaComp f xs p = map f (filter p xs)