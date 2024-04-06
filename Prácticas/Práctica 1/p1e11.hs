foldNat :: Int -> Int -> (Int -> Int -> Int) -> Int
foldNat cb 1 _ = cb
foldNat cb n f = f cb (foldNat cb (n-1) f)

potencia::Int->Int->Int
potencia base exponente = foldNat base exponente (*)