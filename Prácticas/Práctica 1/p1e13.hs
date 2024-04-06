data AB a = Nil | Bin (AB a) a (AB a)

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAb cNil _ Nil = cNil
foldAB cNil cBin (Bin hi r hd) = cBin (foldAB cNil cBin hd) r (foldAB cNil cBin hi)

recAB :: (b -> AB a -> a -> AB a -> b -> b) -> b -> AB a -> b
recAB cBin cNil Nil = cNil
recAB cBin cNil (Bin hi r hd) = cBin (recAB cBin cNil hd) hd r hi (recAB cBin cNil hi)

--recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
--recr f z [] = z
--recr f z (x : xs) = f x xs (recr f z xs)

esNil :: AB a -> Bool
esNil Nil = True
esNil (Bin hi r hd) = False

cantHojas :: AB a -> Int 
cantHojas = foldAB 0 (\hi _ hd -> 1 + hi + hd) 

altura :: AB a -> Int 
altura = foldAB 0 (\hi _ hd -> (max (hi+1) (hd+1))) 

--mejor segun puede salir con foldAB
--mejorSegun :: Ord a => (a -> a -> Bool) -> [a] -> a
--mejorSegun f l = foldr1 (\x y  -> if f x y then x else y) l

mejorSegun :: a -> (a -> a -> Bool) -> AB a -> a
mejorSegun cNil f  = foldAB cNil (\hi r hd -> if (f hi hd && f hi r) then
                                                    hi else (if f hd r then hd else r)) 
--como hacer sin el cNIl

esABB :: Ord a => AB a -> Bool


