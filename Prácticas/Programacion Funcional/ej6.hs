recr :: (a -> [a] -> b -> b) -> b -> [a] -> b 
recr f z [] = z 
recr f z (x:xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a -> [a] -> [a]
--sacarUna e [] = []
--sacarUna e (x:xs) = if e == x then xs else sacarUna e xs 
sacarUna e = recr (\x xs rec -> if e == x then xs else x:rec) []

-- No se puede resolver con fold pues estamos usando la cola de la lista 

insertarOrdenado :: Ord a => a -> [a] -> [a]
--insertarOrdenado e [] = [e]
--insertarOrdenado e (x:xs) = if e < x then (e:x:xs) else  x : insertarOrdenado e xs
insertarOrdenado e = recr(\x xs rec -> if e < x then (e:x:xs) else x : rec) [e]