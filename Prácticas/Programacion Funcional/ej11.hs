data Polinomio a = X
                | Cte a
                | Suma (Polinomio a) (Polinomio a)
                | Prod (Polinomio a) (Polinomio a)

foldPoli :: b -> --X
            (a -> b) -> --Cte 
            (b -> b -> b) -> --Suma
            (b -> b -> b) -> --Prod 
            Polinomio a -> b
foldPoli cX cCte cSuma cProd poli = case poli of 
    X -> cX 
    Cte a -> cCte a
    Suma a b -> cSuma (rec a) (rec b)
    Prod a b -> cProd (rec a) (rec b)
    where rec = foldPoli cX cCte cSuma cProd

evaluar :: Int -> Polinomio Int -> Int 
evaluar x p = foldPoli x id (+) (*) p 