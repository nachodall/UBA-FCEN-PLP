genLista :: a -> (a -> a) -> Integer -> [a]
genLista x f 0 = []
genLista x f cant = x : genLista (f x) f (cant-1)

desdeHasta :: Integer -> Integer -> [Integer]
desdeHasta x y = genLista x (+1) (y-x+1) 

