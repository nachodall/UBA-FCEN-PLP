import Prelude 
--1)null :: [a] -> Bool, me dice si una lista es vacia o no
--  head :: [a] -> a, tail :: [a] -> [a], last :: [a] -> a, es obvio que hacen
--  take :: Int -> [a] -> [a], toma una cantidad de elementos de una lista y los arma en una nueva
--  drop :: Int -> [a] -> [a], toma los n elementos de una lista y los saca
--  (++) :: [a] -> [a] -> [a], concatena dos lista
--  concat :: [[a]] -> [a], agarra una lista de listas y las junta todas
--  (!!!) :: int -> [a] -> a, me da el iesimo elemento de una lista
--  elem :: a -> [a] -> Bool, me dice si un elem esta en la lista o no

--2)
--a)
valorAbsoluto :: Float -> Float
valorAbsoluto n | n >= 0 = n 
                | otherwise = -n 
--b)
bisiesto :: Int -> Bool 
bisiesto a | mod a 400 == 0 = True
           | mod a 4 == 0 && mod a 100 /= 0 = True
           | otherwise = False
--c)
factorial :: Int -> Int
factorial n | n == 0 = 1
            | otherwise = n * factorial n-1

--d)
cantDivisoresPrimos :: Int -> Int 
cantDivisoresPrimos n = cantDivisoresPrimosAux n 2

cantDivisoresPrimosAux :: Int -> Int -> Int 
cantDivisoresPrimosAux n k | n == k = 0
                           | esPrimo k && mod n k == 0 = 1 + cantDivisoresPrimosAux n (k+1)
                           | otherwise = cantDivisoresPrimosAux n (k+1)

esPrimo :: Int -> Bool 
esPrimo n = esPrimoAux n 2

esPrimoAux :: Int -> Int -> Bool 
esPrimoAux n k | n == k = True
               | mod n k == 0 = False
               | otherwise = esPrimoAux n (k+1)


--3)
--a)
inverso :: Float -> Maybe Float 
inverso n | n == 0 = Nothing 
          | otherwise = Just (1 / n)

aEntero :: Either Int Bool -> Int --pasar x ghci por ej aEntero (Right True) aEntero (Left 5)
aEntero (Left x) = x 
aEntero (Right True) = 1
aEntero (Right False) = 0

--4)
--a)
limpiar :: String -> String -> String
limpiar _ [] = []
limpiar s1 s2 | elem (head s2) s1 = limpiar s1 (tail s2) 
limpiar s1 s2 | otherwise = head s2 : limpiar s1 (tail s2)

--b)
difPromedio :: [Float] -> [Float]
difPromedio l = difPromedioAux l promedioGeneral 
    where
        promedioGeneral = sum l / fromIntegral (length l) --hay q parsear a float el len

difPromedioAux :: [Float] -> Float -> [Float]
difPromedioAux [] _ = []
difPromedioAux (x:xs) p = (x - p) : difPromedioAux xs p

todosIguales :: [Int] -> Bool 
todosIguales l | null (tail l) = True 
               | notElem (head l) (tail l) = False 
               | otherwise = todosIguales (tail l)

--5
data AB a = Nil | Bin (AB a) a (AB a)
--a) 
vacioAB :: AB a -> Bool
vacioAB Nil = True 

--b)
negacionAB :: AB Bool -> AB Bool 
negacionAB a | vacioAB a = a 
negacionAB (Bin hi a hd) = Bin (negacionAB hi) (not a) (negacionAB hd) --Bin viene a ser como una especie de constructor?
--c)
productoAB :: AB Int -> Int
productoAB Nil = 1 
productoAB (Bin hi a hd) = a * (productoAB hi) * (productoAB hd)

