--i. Definir la función permutaciones :: [a] -> [[a]], que dada una lista devuelve todas sus permutaciones. Se recomienda utilizar concatMap :: (a -> [b]) -> [a] -> [b], y también take y drop
--permutaciones :: [a] -> [[a]]
--permutacions [] = [] 
--permutaciones (x:xs) = concatMap (\x -> x : permutaciones xs) xs

[] => []
[1] => [1]
[2, 1] => [[1, 2], [2, 1]]
[3, 2, 1] => [[1, 2, 3], [2, 1, 3], [2, 3, 1], 
              [1, 3, 2], [3, 2, 1], [3, 1, 2]]

[1,2] => [3,1,2], [1,3,2], [1,2,3]
[[[3,1,2], [1,3,2], [1,2,3]], [...]]

concatMap [[1,2,3], [4,5]] = [1,2,3,4,5]

--ii. 
partes :: [a] -> [[a]]
partes [] = [[]]  -- Caso base: la lista vacía tiene una sublista, que es la lista vacía misma
partes (x:xs) = concatMap (\ys -> [x:ys, ys]) (partes xs) 


