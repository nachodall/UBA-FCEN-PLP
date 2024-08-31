foldNat :: (Integer -> b -> b) -> b -> Integer -> b 
foldNat cNat cCero 0 = cCero
foldNat cNat cCero n = cNat n (foldNat cNat cCero (n-1)) 

potencia :: Integer -> Integer -> Integer
potencia base exponente = foldNat (\_ rec -> base*rec) 1 exponente