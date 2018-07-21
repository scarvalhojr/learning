
reverse' :: [a] -> [a]
reverse' = acc []
  where acc :: [a] -> [a] -> [a]
        acc as []     = as
        acc as (x:xs) = acc (x:as) xs

-- main = putStrLn $ show (head $ reverse' [1..100000000])

data Tree = Leaf Int | Node Tree Tree
  deriving Show

flatten :: Tree -> [Int]
flatten = flat []
  where flat :: [Int] -> Tree -> [Int]
        flat acc (Leaf x)   = x : acc
        flat acc (Node l r) = flat (flat acc r) l
