
import Data.List (transpose, maximum)

size :: Int
size = 3

data Player = O | B | X
              deriving (Eq, Ord, Show)

type Grid = [[Player]]

empty :: Grid
empty = replicate size (replicate size B)

next :: Player -> Player
next O = X
next X = O
next B = B

full :: Grid -> Bool
full = all (/= B) . concat

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size - 1]]

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ diags)
           where line  = all (== p)
                 rows  = g
                 cols  = transpose g
                 diags = [diag g , diag (map reverse g)]

won :: Grid -> Bool
won g = wins O g || wins X g

valid :: Grid -> Int -> Bool
valid g i = i >= 0 && i < size ^ 2 && concat g !! i == B

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

move :: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i then [chop size (xs ++ [p] ++ ys)] else []
             where (xs, B : ys) = splitAt i (concat g)

moves :: Grid -> Player -> [Grid]
moves g p
    | won g     = []
    | full g    = []
    | otherwise = concat [move g i p | i <- [0..size ^ 2 - 1]]

--

data Tree a = Node a [Tree a]
              deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

-- Exercice 1

maxdepth :: Tree a -> Int
maxdepth (Node _ []) = 0
maxdepth (Node _ xs) = 1 + maximum (map maxdepth xs)

countNodes :: Tree a -> Int
countNodes (Node _ xs) = 1 + sum (map countNodes xs)
