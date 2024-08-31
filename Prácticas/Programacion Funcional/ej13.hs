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

cantHojas :: AB a -> Int
cantHojas = recAB  0 (\i r d reci recd -> if esNil i && esNil d then 1 else reci + recd)

-- ramas??

espejo :: AB a -> AB a 
espejo = foldAB Nil (\i r d -> Bin d r i)

