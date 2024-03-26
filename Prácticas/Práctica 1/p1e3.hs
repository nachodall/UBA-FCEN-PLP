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
