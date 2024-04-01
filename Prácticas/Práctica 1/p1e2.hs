--2.1) Definir la función curry, que dada una función de dos argumentos, devuelve su equivalente currificada
curry' :: ((a,b) -> c) -> a -> b -> c
curry' f x y = f (x,y)
--pruebas
suma :: (Int, Int) -> Int
suma (x, y) = x + y

sumaCurrificada :: Int -> Int -> Int
sumaCurrificada = curry' suma

--2.2)
uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f (x,y) = f x y

restaCurrificada :: Int -> Int -> Int
restaCurrificada x y = x - y

restaUncurrificada :: (Int, Int) -> Int
restaUncurrificada = uncurry' restaCurrificada

--2.3) ?????