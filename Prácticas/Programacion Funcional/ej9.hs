sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat [] _ = []
sumaMat (x:xs) (y:ys) = zipWith (+) x y : sumaMat xs ys

-- ????
trasponer :: [[Int]] -> [[Int]]
trasponer = foldr (zipWith (:)) (repeat [])
