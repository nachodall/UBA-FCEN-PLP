-- 1 
permutaciones :: [a] -> [[a]]
--permutaciones [] = [[]]
--permutaciones (x:xs) =  (concatMap (\ys -> [take n ys ++ [x] ++ drop n ys | n <- [0..length ys]])) (permutaciones xs)
permutaciones =  foldr (\x rec -> (concatMap (\ys -> [take n ys ++ [x] ++ drop n ys | n <- [0..length ys]])) rec) [[]]

-- 2
partes :: [a] -> [[a]]
partes = foldr (\x rs -> rs ++ map (x:) rs) [[]]

-- 3 
prefijos :: [a] -> [[a]]
prefijos = foldl (\rs x -> rs ++ [last rs ++ [x]]) [[]]

-- 4 x`
sublistas :: [a] -> [[a]]
--sublistas [] = [[]]
--sublistas (x:xs) = prefijos (x:xs) ++ sublistas xs
sublistas = recr (\x xs rec -> prefijos (x:xs) ++ rec) [[]]

recr :: (t1 -> [t1] -> t2 -> t2) -> t2 -> [t1] -> t2
recr _ z [] = z 
recr f z (x:xs) = f x xs (recr f z xs)
