max2 :: Ord a => (a, a) -> a
max2 (x, y) | x >= y = x
            | otherwise = y

normaVectorial :: Floating a => (a, a) -> a
normaVectorial (x, y) = sqrt (x^2 + y^2)

subtract_ :: Integer -> Integer -> Integer
subtract_ = flip (-)

predecesor :: Integer -> Integer 
predecesor n = subtract_ 1 n

evaluarEnCero :: (Integer -> a) -> a
evaluarEnCero = \f -> f 0 

dosVeces :: (a -> a) -> a -> a
dosVeces = \f -> f . f

flipAll :: [a -> b -> c] -> [b -> a -> c]
flipAll = map flip  

{-
    Notar que flip flip no es lo mismo que dejar los argumentos en el lugar donde estaban. 
    Eso seria flip . flip  

    flip flip, intercambia los argumentos de la funcion flip, es decir aplicamos flip a la funcion flip 
    llamemos flip' a la de la derecha. 
    Tenemos: 
    flip :: (a->b->c) -> b -> c -> a
    flip' :: (a'->b'->c') -> b' -> c' -> a'
    O sea que en (a ->b -> c) entra flip'. Haciendo pattern matching y recordando como se asocian los tipos en Haskell: 
    a = (a'->b'->c')
    b = b' 
    c = c'-> a' 

    entonces como aplicar flip tiene tipo b -> a -> c, nos queda:
-}
flipRaro :: b' -> (a'->b'->c') -> a' -> c'
flipRaro = flip flip