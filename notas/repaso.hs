foldrr :: (a -> b -> b) -> b -> [a] -> b
foldrr f z []     = z
foldrr f z (x:xs) = f x (foldrr f z xs)

foldll :: (b -> a -> b) -> b -> [a] -> b
foldll _ acc []     = acc
foldll f acc (x:xs) = foldll f (f acc x) xs

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z []       = z
recr f z (x : xs) = f x xs (recr f z xs)

data Polinomio a = X
                 | Cte a
                 | Suma (Polinomio a) (Polinomio a)
                 | Prod (Polinomio a) (Polinomio a)


evaluar n poli = case poli of
       X -> n
       Cte k -> k
       Suma p q -> evaluar n p + evaluar n q
       Prod p q -> evaluar n p * evaluar n q
-- evaluar n = foldPoli n id (+) (*)


foldPoli :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPoli cX cCte cSuma cProd poli = case poli of
    X -> cX
    Cte k -> cCte k
    Suma p q -> cSuma (rec p) (rec q)
    Prod p q -> cProd (rec p) (rec q)
  where rec = foldPoli cX cCte cSuma cProd

flip' :: (a -> b -> c) -> b -> a -> c
-- flip' f y x = f x y
flip' f = \y x -> f x y  

data RoseTree a = Rose a [RoseTree a]

tamaño :: RoseTree a -> Int
tamaño (Rose x hijos) = 1 + sum (map tamaño hijos)
-- tamaño = foldRT (\_ recs -> 1 + sum recs)


foldRT :: (a -> [b] -> b) -> RoseTree a -> b
foldRT f (Rose x hijos) = f x (map (foldRT f) hijos)

data Componente = Contenedor
                | Motor 
                | Escudo
                | Cañon 
                deriving Eq 

data NaveEspacial = Modulo Componente NaveEspacial NaveEspacial
                  | Base Componente
                  deriving Eq 
                

foldNave :: (Componente -> b -> b -> b) -> (Componente -> b) -> NaveEspacial -> b
foldNave cModulo cBase nave = case nave of 
    Modulo c n1 n2 -> cModulo c (recur n1) (recur n2)
    Base c -> cBase c
    where recur = foldNave cModulo cBase 


recNave :: (Componente -> NaveEspacial -> NaveEspacial -> b -> b -> b) -> (Componente -> b) -> NaveEspacial -> b
recNave cRec cBase (Base c) = cBase c 
recNave cRec cBase (Modulo c n1 n2) = cRec c n1 n2 (recur n1) (recur n2)
    where recur = recNave cRec cBase 

foldNave2 :: (Componente -> b -> b -> b) -> (Componente -> b) -> NaveEspacial -> b
foldNave2 cRec cBase nave = recNave (\c _ _ -> cRec c ) cBase nave 

espejo :: NaveEspacial -> NaveEspacial 
espejo nave = foldNave (\c rec1 rec2 -> Modulo c rec2 rec1) Base nave 

esSubnave :: NaveEspacial -> NaveEspacial -> Bool
esSubnave n1 n2 = recNave (\c head1 head2 rec1 rec2 -> 
                           n1 == head1 || n1 == head2 || rec1 || rec2) 
                            (const False) n2 

truncar :: NaveEspacial -> Integer -> NaveEspacial 
truncar nave = foldNave (\c rec1 rec2 -> \i ->
                            if i == 0 
                            then (Base c)
                            else (Modulo c (rec1 (i-1)) (rec2 (i-1)))) 
                            (\c -> \i -> Base c) nave

data AB a = Nil 
          | Bin (AB a) a (AB a) 

foldAB :: (b -> a -> b -> b) -> b -> AB a -> b
foldAB cBin cNil arbol = case arbol of 
    Bin i r d -> cBin (foldAB cBin cNil i) r (foldAB cBin cNil d)
    Nil -> cNil 

recAB :: (AB a -> a -> AB a -> b -> b -> b) -> b -> AB a -> b --le entra su estructura y despues la recursiones
recAB cBin cNil Nil = cNil 
recAB cBin cNil (Bin i r d) = cBin i r d (recAB cBin cNil i) (recAB cBin cNil d)

esNil :: AB a -> Bool 
esNil Nil = True 
esNil (Bin i r d) = False

altura :: AB a -> Integer 
altura  = foldAB (\i r d -> 1 + max i d) 0

cantNodos :: AB a -> Integer 
cantNodos  = foldAB (\i r d -> 1 + i + d) 0 

mejorSegun :: (a -> a -> Bool) -> AB a -> a 
mejorSegun f (Bin a b c) = foldAB (\i r d -> if f i d && f i r then i 
                        else (if f r d then r else d)) b (Bin a b c)

esABB :: Ord a => AB a -> Bool 
esABB = recAB aux True
    where aux i r d reci recd | esNil i && esNil d = True 
                              | esNil i = esABB d 
                              | esNil d = esABB i 
                              | otherwise = raiz i <= r && raiz d >= r && reci && recd

raiz :: AB a -> a
raiz (Bin i r d) = r

cantHojas :: AB a -> Integer 
cantHojas = foldAB (\i r d -> if i == 0 && d == 0 then 1 else 0 + i + d ) 0

espejoAB :: AB a -> AB a
espejoAB = foldAB (\ i r d -> Bin d r i) Nil 

ramas :: AB a -> [[a]]
ramas = foldAB aux []
    where aux i r d | esVacia i && esVacia d = [[r]]
                    | esVacia d = map (r:) i 
                    | esVacia i = map (r:) d
                    | otherwise = map (r:) i ++ map(r:) d

esVacia :: [a] -> Bool 
esVacia [] = True 
esVacia (x:xs) = False

foldRecAb :: (b -> a -> b -> b) -> b -> AB a -> b
foldRecAb cBin cNil = recAB (\ _ r _ i d -> cBin i r d) cNil

takeN :: Int -> [a] -> [a]
takeN = flip (foldr (\x rec i ->
        if i == 0 then []
        else x : rec (i-1)
    ) (const []))

permutaciones :: [a] -> [[a]]
permutaciones =
    foldr (\x rs ->
        concatMap (\r ->
            map (insert x r) [0..length r]
        ) rs
    ) [[]]
    where insert x r i = drop i r ++ [x] ++ take i r

partes :: [a] -> [[a]]
partes = foldr (\x rs -> rs ++ map (x:) rs) [[]]
-- partes = foldl (\rs x -> rs ++ map (\r -> r ++ [x]) rs) [[]]

prefijos :: [a] -> [[a]]
prefijos = foldl (\rs x -> rs ++ [last rs ++ [x]]) [[]]

sublistas :: [a] -> [[a]]
sublistas = recr (\x xs r -> map (x :) (prefijos xs) ++ r) [[]]
-- sublistas [] = [[]]
-- sublistas (x:xs) = map (x :) (prefijos xs) ++ sublistas xs
sum :: [Integer] -> Integer
sum = foldr (+) 0

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem e = foldr ((||) . (== e)) False

(++) :: [Integer] -> [Integer] -> [Integer]
(++) = flip (foldr (:))

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\x r -> if f x then x:r else r) []

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []

curry :: ((a, b) -> c) -> a -> b -> c
curry f a b = f (a, b)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b

-- Esta sería la idea, pero no se puede hacer en Haskell.
curryN :: ((a, bs...) -> c) -> a -> bs... -> c
curryN f a bs = f (a, bs...)

mejorSegún :: (a -> a -> Bool) -> [a] -> a
mejorSegún f = foldr1 (\x y -> if f x y then x else y)

sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldl (\r x -> r ++ (if null r then [x] else [x + last r])) []

sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (-) 0

sumaAlt2 :: Num a => [a] -> a
sumaAlt2 = foldl (flip (-)) 0

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) =
    if null xs then [x]
    else x : elementosEnPosicionesPares (tail xs)
-- No es recursión estructural porque en el caso else,
-- la recursión se hace sobre tail xs en vez de xs entero.
-- Se "descartan" elementos en la recursión sobre xs.

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys ->
    if null ys then x : entrelazar xs []
    else x : head ys : entrelazar xs (tail ys)

entrelazar2 :: [a] -> ([a] -> [a])
entrelazar2 = foldr (\x fr ys ->
        if null ys then x : fr []
        else x : head ys : fr (tail ys)
    ) id

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna y = recr (\x xs r -> if x == y then xs else x:r) []

-- No podemos implementar sacarUna con foldr porque no hay "memoria".
-- Cada vez que procesamos un elemento, no sabemos si ya sacamos algún
-- elemento anterior o no. Dicho de otra manera, con foldr no podemos
-- hacer un return temprano y "abortar" el resto del fold.

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado y = recr (\x xs r ->if y <= x then y:x:xs else x:r) [y]

genLista :: a -> (a -> a) -> Int -> [a]
genLista n f 0 = []
genLista n f i = n : genLista (f n) f (i-1)

-- Con funciones del preludio.
-- genLista n f i = take i (iterate f n)

desdeHasta :: Int -> Int -> [Int]
desdeHasta i j = genLista i (+1) (j-i+1)

mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = map (uncurry f)

armarPares :: [a] -> [b] -> [(a, b)]
armarPares = foldr (\x rec ys ->
        if null ys then []
        else (x, head ys) : rec (tail ys)
    ) (const [])

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f = foldr (\x rec ys ->
        if null ys then []
        else f x (head ys) : rec (tail ys)
    ) (const [])

sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = zipWith (zipWith (+))

trasponer :: [[Int]] -> [[Int]]
trasponer = foldl (\rec x -> zipWith (++) rec (map (:[]) x)) (repeat [])

generate :: ([a] -> Bool) -> ([a] -> a) -> [a]
generate stop next = generateFrom stop next []

generateFrom :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom stop next xs
    | stop xs = init xs
    | otherwise = generateFrom stop next (xs ++ [next xs])

generateBase :: ([a] -> Bool) -> a -> (a -> a) -> [a]
generateBase stop base next = generate stop (\xs -> if null xs then base else next (last xs))

factoriales :: Int -> [Int]
factoriales n = generate (\xs -> length xs > n) (\xs -> if null xs then 1 else last xs * (length xs + 1))

iterateN :: Int -> (a -> a) -> a -> [a]
iterateN n f x = generateBase (\xs -> length xs > n) x f

generateFrom2 :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom2 stop next xs = last (takeWhile (not . stop) (iterate (\xs -> xs ++ [next xs]) xs))

foldNat :: (Integer -> b -> b) -> b -> Integer -> b
foldNat f z 0 = z
foldNat f z n = f n (foldNat f z (n-1))

potencia :: Integer -> Integer -> Integer
potencia x = foldNat (\_ rec -> rec * x) 1

data Polinomio a = X
    | Cte a
    | Suma (Polinomio a) (Polinomio a)
    | Prod (Polinomio a) (Polinomio a)
    deriving(Show)

evaluar :: Num a => a -> Polinomio a -> a
evaluar x X = x
evaluar _ (Cte c) = c
evaluar x (Suma p q) = evaluar x p + evaluar x q
evaluar x (Prod p q) = evaluar x p * evaluar x q

-- X^2 + X + 1
-- evaluar 1 (Suma (Prod X X) (Suma X (Cte 1)))

foldPoli ::
    b               -- X
    -> (a -> b)       -- Cte
    -> (b -> b -> b)  -- Suma
    -> (b -> b -> b)  -- Prod
    -> Polinomio a
    -> b

foldPoli cX cCte cSuma cProd t = case t of
    X -> cX
    Cte c -> cCte c
    Suma p q -> cSuma (rec p) (rec q)
    Prod p q -> cProd (rec p) (rec q)
    where rec = foldPoli cX cCte cSuma cProd

evaluarFoldeado :: Num a => a -> Polinomio a -> a
evaluarFoldeado x = foldPoli x id (+) (*)

