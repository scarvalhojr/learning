and' :: [Bool] -> Bool
and' [] = True
and' (False:xs) = False
and' (True:xs) = and' xs 

concat' :: [[a]] -> [a]
concat' [] = []
concat' [[]] = []
concat' [[x]] = [x]
-- ???
--concat' [x:xs] = x : concat xs 
--concat' [x:xs] = x : concat' [xs]
--concat' (x:xs):ys = x : concat' ([xs] : ys) 

replicate' :: Int -> a -> [a]
replicate' k x
    | k <= 0    = []
    | otherwise = x : replicate' (k - 1) x

(!!!) :: [a] -> Int -> a
(!!!) (x:xs) k
    | k == 0    = x
    | k > 0     = (!!!) xs (k - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    | x == a    = True
    | otherwise = elem' a xs

merge' :: Ord a => [a] -> [a] -> [a]
merge' x [] = x
merge' [] y = y
merge' (x:xs) (y:ys)
    | x <= y    = x : merge' xs (y:ys)
    | otherwise = y : merge' (x:xs) ys

halve :: [a] -> ([a],[a])
halve xs = (take len xs, drop len xs)
    where len = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x] 
msort xs = merge' (msort left) (msort right)
    where (left, right) = halve xs 

