import Data.Char

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects m = [x | x <- [1..m], isPerfect x]
    where isPerfect x = sum (factors x) == 2 * x

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x (zip xs [0..])

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]

shift :: Int -> Char -> Char
shift n c
    | isLower c     = shiftchar n c 'a'
    | isUpper c     = shiftchar n c 'A'
    | otherwise     = c
    where
        shiftchar n c b = int2let ((let2int c b + n) `mod` 26) b
        let2int c b = ord c - ord b
        int2let i b = chr (i + ord b)

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
