import Control.Arrow (Arrow(first))
--i. Redefinir usando foldr las funciones sum, elem, (++), filter y map.
sum_foldr :: Num a => [a] -> a
sum_foldr l = foldr (+) 0 l

elem_foldr :: (Eq a) => a -> [a] -> Bool
elem_foldr e l = foldr (\x acc -> if x == e then True else acc) False l -- seguimos con el esquma foldr f casoBase lista

filter_foldr :: (a -> Bool) -> [a] -> [a]
filter_foldr pred l = foldr (\x acc -> if pred x then x:acc else acc) [] l

map_foldr :: (a -> b) -> [a] -> [b]
map_foldr f l = foldr (\x acc -> f x : acc) [] l



--Definir la función mejorSegún :: (a -> a -> Bool) -> [a] -> a, que devuelve el máximo elemento
--de la lista según una función de comparación, utilizando foldr1. Por ejemplo, maximum = mejorSegún(>).
mejorSegun :: Ord a => (a -> a -> Bool) -> [a] -> a
mejorSegun f l = foldr1 (\x y  -> if f x y then x else y) l

--Definir la función sumasParciales :: Num a => [a] -> [a], que dada una lista de números devuelve
--otra de la misma longitud, que tiene en cada posición la suma parcial de los elementos de la lista original
--desde la cabeza hasta la posición actual. Por ejemplo, sumasParciales [1,4,-1,0,5] ❀ [1,5,4,4,9].
sumasParciales :: Num a => [a] -> [a]
sumasParciales xs = tail (foldl (\acc x -> acc ++ [last acc + x]) [0] xs) --paso primero acc q x porque fold l pide primero el acumulador
--use foldl para ir haciendo de izq a derecha y el tail para q no me cuente el 0

--Definir la función sumaAlt, que realiza la suma alternada de los elementos de una lista. Es decir, da como
--resultado: el primer elemento, menos el segundo, más el tercero, menos el cuarto, etc. Usar foldr.
sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (\x acc -> x - acc) 0

-- sumaAlt [4, 1, 2, 3]
-- 4 - (1 - (2 - (3 - 0))) = 4 + (-1) + 2 + (-3) + 0

--Hacer lo mismo que en el punto anterior, pero en sentido inverso (el último elemento menos el anteúltimo,
--etc.). Pensar qué esquema de recursión conviene usar en este caso.

