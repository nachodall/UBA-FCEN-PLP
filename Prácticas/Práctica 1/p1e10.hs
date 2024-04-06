import Distribution.Simple.Command (OptDescr(BoolOpt))
generate :: ([a] -> Bool) -> ([a] -> a) -> [a]
generate stop next = generateFrom stop next []

generateFrom:: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom stop next xs | stop xs = init xs
                          | otherwise = generateFrom stop next (xs ++ [next xs])

generateBase :: ([a] -> Bool) -> a -> (a -> a) -> [a]
generateBase stop e next = generate stop (\l -> next(last l))

factoriales :: Int -> [Int]
factoriales i = generate stop next 
    where 
        stop :: [Int] -> Bool
        stop l = length l == i + 1 --detiene la lista cuando tiene length i+1

        next :: [Int] -> Int 
        next [] = 1
        next l = (length l + 1) * last l

iterateN :: Int -> (a -> a) -> a -> [a]
iterateN n f x = generateBase (\l -> length l >= n) x f
  
--consultar generate base y iterate n