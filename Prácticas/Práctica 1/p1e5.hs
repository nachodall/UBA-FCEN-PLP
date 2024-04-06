--1) 
elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares xs = foldr (\(i, x) acc -> if even i then x : acc else acc) [] (zip [0..] xs)
--el zip me genera una tupla con el valor de la misma pos de cada lista
--igual esta no era recursion estructural

entrelazar :: [a] -> [a] -> [a]
entrelazar = foldr(\x f -> --en f se guarda el result de la recursion en este caso es una f xq hacemos de cuenta q hay solo un argumento
                        (\ys -> if null ys then [x] else x : f (tail ys))) id


