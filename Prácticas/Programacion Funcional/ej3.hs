--1
sumConFoldr :: Num a => [a] -> a 
sumConFoldr = foldr (+) 0

elemConFoldr :: Eq a => [a] -> a -> Bool 
elemConFoldr xs e = foldr (\x acc -> (x==e) || acc) False xs 

filterConFoldr :: (a -> Bool) -> [a] -> [a]
filterConFoldr pred  = foldr (\x acc -> if pred x then x:acc else acc) [] 

mapConFoldr :: (a->a) -> [a] -> [a]
mapConFoldr f  = foldr (\x acc -> f x : acc) [] 

--2 
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x acc -> if f x acc then x else acc)

--3
sumaAlt :: Num a => [a] -> a 
sumaAlt = foldr (-) 0

{- 
sumaAlt [1,2,4,5] = 1 + sumaAlt[2,3,4,5] = 1 - 2 + sumaAlt[3,4,5] = 1 - 2 + 3 + sumaAlt[4,5] = 1 - 2 + 3 - 4 + sumaAlt[5] = 1 - 2 + 3 - 4 + 5 = 3
-}

--4 
sumaAlt2 :: Num a => [a] -> a 
sumaAlt2 = foldl (flip (-)) 0 