halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
    where n = length xs `div` 2

third :: [a] -> a
third xs = head (tail (tail xs))

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' (_:_:x:xs) = x

safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs
    | null xs   = []
    | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (x:xs) = xs

(|||) :: Bool -> Bool -> Bool
True  ||| True  = True
False ||| True  = True
True  ||| False = True
False ||| False = False

(||||) :: Bool -> Bool -> Bool
False |||| False = False
_     |||| _     = True

(|||||) :: Bool -> Bool -> Bool
False ||||| x = x
_     ||||| _ = True

(||||||) :: Bool -> Bool -> Bool
a |||||| b
    | a == b    = a
    | otherwise = True

(&&&) :: Bool -> Bool -> Bool
a &&& b = if a then if b then True else False else False

(&&&&) :: Bool -> Bool -> Bool
a &&&& b = if a then b else False

luhnDouble :: Int -> Int
luhnDouble n = (\x -> if x > 9 then (x - 9) else x) (2 * n)

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = tot `mod` 10 == 0
    where tot = b + d + sum (map luhnDouble [a,c])

