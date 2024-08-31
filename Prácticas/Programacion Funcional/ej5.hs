entrelazar :: [a] -> [a] -> [a]
entrelazar = foldr (\x rec -> \ys -> if null ys then
                                        x : rec []
                                        else  x : head ys : rec (tail ys)) id 