
-- Exercise 2

rmfirst :: Eq a => a -> [a] -> [a]
rmfirst x []     = []
rmfirst x (y:ys)
    | x == y    = ys
    | otherwise = y : rmfirst x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _      = True
isChoice _ []      = False
isChoice (x:xs) ys = elem x ys && isChoice xs (rmfirst x ys)
