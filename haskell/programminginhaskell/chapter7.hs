import Data.Char
import Data.List

-- Binary string transmitter

type Bit = Int

--bin2int :: [Bit] -> Int
--bin2int bits = sum [w * b | (w,b) <- zip weights bits]
--    where weights = iterate (*2) 1

bin2int :: [Bit] -> Int
bin2int = foldr (\ x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 []   = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode

-- Voting algorithms: first past the post

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

-- Voting algorithms: alternative vote

ballots :: [[String]]
ballots = [["Red","Green"],
           ["Blue"],
           ["Green","Red","Blue"],
           ["Blue","Green","Red"],
           ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
                [c]    -> c
                (c:cs) -> winner' (elim c bs)

-- Exercise 1

-- map f (filter p xs)

-- Exercise 2

all'' :: (a -> Bool) -> [a] -> Bool
all'' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x       = x : takeWhile' p xs
    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
    | p x       = dropWhile' p xs
    | otherwise = x : xs

-- Exercise 3

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\ x y -> f x : y) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\ x y -> if p x then x : y else y) []

-- Excersive 4

dec2int :: [Int] -> Int
dec2int = foldl (\ y x -> x + 10 * y ) 0

-- Excersive 5

curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f = \ x y -> f (x,y)

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f = \ (x,y) -> f x y

-- Excersive 6

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
    | p x       = []
    | otherwise = h x : unfold p h t (t x)

int2bin' :: Int -> [Bit]
int2bin' = unfold (==0) (`mod` 2) (`div` 2)

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold ((==) 0 . length) (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold ((==) 0 . length) (f . head) (tail)

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\x -> False) id f

-- Exercise 7

parity :: [Bit] -> Bit
parity = foldl (\x y -> (x + y) `mod` 2) 0

addparity :: [Bit] -> [Bit]
addparity xs = parity xs : xs

encode' :: String -> [Bit]
encode' = concat . map (addparity . make8 . int2bin . ord)

rmparity :: [Bit] -> [Bit]
rmparity (x:xs)
    | x == parity xs    = xs
    | otherwise         = error "Parity check failed."

chop9 :: [Bit] -> [[Bit]]
chop9 []   = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

decode' :: [Bit] -> String
decode' = map (chr . bin2int . rmparity) . chop9

transmit' :: String -> String
transmit' = decode' . channel . encode'

-- Exercise 8

faultychannel :: [Bit] -> [Bit]
faultychannel = tail

brokentransmit = decode' . faultychannel . encode'

-- Exercise 9

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : altMap g f xs

-- Exercise 10

luhnDouble :: Int -> Int
luhnDouble n = (\x -> if x > 9 then (x - 9) else x) (2 * n)

luhn :: [Int] -> Bool
luhn xs = total `mod` 10 == 0
    where total = sum (altMap luhnDouble id xs)
