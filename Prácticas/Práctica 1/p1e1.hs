import Distribution.Simple.Utils (xargs)
--1-a)
max2 :: Ord a => (a,a) -> a --no esta currificada
max2 (x, y) | x >= y = x
            | otherwise = y
--1-b)
normaVectorial :: Floating a => (a, a) -> a --no esta currificada
normaVectorial (x, y) = sqrt (x^2 + y^2)
--1-c)
--subtract :: Num a => a -> a -> a --esta currificada
--subtract = flip (-)
--1-d)
--predecesor :: Num a => a -> a -> a --esta currificada
--predecesor  = subtract' 1

evaluarEnCero :: Num a => (a -> b) -> b --evaluarEnCero (\x -> x+1)
evaluarEnCero = \f -> f 0

dosVeces :: (a -> a) -> (a -> a) --los parentesis los puse para entender yo q "devuelve" una funcion por que no puede ser a -> b
dosVeces = \f -> f . f

flipAll :: [a -> b -> c] -> [b -> a -> c]
flipAll = map flip 

flipRaro :: b -> (a -> b -> c) -> a -> c --por q es este tipo??
flipRaro = flip flip