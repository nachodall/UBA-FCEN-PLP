recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

--a)
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs r -> if x == e then xs else x:r) []

--b) foldr no es adecuado xq al ser recursion estructural no me deja usar el xs
insertarOrdenado :: Ord a => a -> [a] -> [a] 
insertarOrdenado e = recr (\x xs r -> if x >= e then e:x:xs else x:r) [e]