uncurry :: (a->b->c) -> (a,b) -> c
uncurry f (x,y) = f x y 

curry :: ((a,b) -> c) -> a -> b -> c 
curry f x y = f (x,y)

-- curryN??? 