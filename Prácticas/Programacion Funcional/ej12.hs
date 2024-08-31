data AB a = Nil | Bin (AB a) a (AB a)

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b 
foldAB cNil cBin arbol = case arbol of 
    Nil -> cNil 
    Bin i r d -> cBin (rec i) r (rec d)
    where rec = foldAB cNil cBin 

recAB :: b -> 
         (AB a -> a -> AB a -> b -> b -> b) -> -- Aca puedo usar el subarbol ademas de su llamado recursivo
         AB a -> b 
recAB cNil cBin arbol = case arbol of
    Nil -> cNil 
    Bin i r d -> cBin i r d (rec i) (rec d)
    where rec = recAB cNil cBin  

esNil :: AB a -> Bool 
esNil arbol = case arbol of 
    Nil -> True 
    Bin i r d -> False 

altura :: AB a -> Int 
altura = foldAB 0 (\i r d -> 1 + max i d)


cantNodos :: AB a -> Int 
cantNodos = foldAB 0 (\i r d -> 1 + i + d)

mejorSegun :: Num a => (a -> a -> Bool) -> AB a -> a -- le puse num a para poder inventar algun caso base para nil, pues nadie es el mejor en nil
mejorSegun f = foldAB 0 (\i r d -> if f i d && f i r then i 
                                     else (if f d r then d else r)) 

esABB :: Ord a => AB a -> Bool 
esABB = recAB True aux 
    where aux i r d reci recd | esNil i && esNil d = True 
                              | esNil i = esABB d
                              | esNil d = esABB i 
                              | otherwise = raiz i < r && raiz d > r && reci && recd

raiz :: AB a -> a
raiz (Bin i r d) = r
