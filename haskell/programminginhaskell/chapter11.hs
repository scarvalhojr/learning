
import Data.List (transpose, minimum, maximum)
import Data.Char (isDigit)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

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

turn :: Grid -> Player
turn g = if os <= xs then O else X
         where os = length (filter (== O) ps)
               xs = length (filter (== X) ps)
               ps = concat g

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

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
    | wins O g  = Node (g, O) []
    | wins X g  = Node (g, X) []
    | otherwise = Node (g, B) []
minimax (Node g ts)
    | turn g == O   = Node (g, minimum ps) ts'
    | turn g == X   = Node (g, maximum ps) ts'
    where ts' = map minimax ts
          ps  = [p | Node (_,p) _ <- ts']

depth :: Int
depth = 9

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g', p') _ <- ts, p' == best]
               where tree = prune depth (gametree g p)
                     Node (_, best) ts = minimax tree

--

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave _ []     = []
interleave _ [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where beside = foldr1 (zipWith (++))
                bar    = replicate 3 "|"

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
          where bar = [replicate ((size * 4) - 1) '-']

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int,Int) -> IO ()
goto (x,y) = putStr("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                       return (read xs)
                   else
                       do putStrLn "ERROR: Invalid number."
                          getNat prompt

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

tictactoe1P :: IO ()
tictactoe1P = do start empty
                 play1P empty O

tictactoe2P :: IO ()
tictactoe2P = do start empty
                 play2P empty O

start :: Grid -> IO ()
start g = do hSetBuffering stdout NoBuffering
             cls
             goto (1,1)
             putGrid g

end :: Grid -> Player -> IO Bool
end g p
    | wins O g  = do putStrLn "Player O wins!\n"
                     return True
    | wins X g  = do putStrLn "Player X wins!\n"
                     return True
    | full g    = do putStrLn "It's a draw!\n"
                     return True
    | otherwise = return False

play2P :: Grid -> Player -> IO ()
play2P g p = do end <- end g p
                if end then
                    return ()
                else
                    do (g', p') <- letPlay g p
                       play2P g' p'

play1P :: Grid -> Player -> IO ()
play1P g p = do end <- end g p
                if end then
                    return ()
                else
                    do if p == O then
                            do (g', p') <- letPlay g p
                               play1P g' p'
                       else
                            do putStr "Player X is thinking...\n"
                               let g' = bestmove g p
                               putGrid g'
                               play1P g' (next p)

letPlay :: Grid -> Player -> IO (Grid, Player)
letPlay g p = do i <- getNat (prompt p)
                 case move g i p of
                     []   -> do putStrLn "ERROR: Invalid move."
                                return (g, p)
                     [g'] -> do putGrid g'
                                return (g', next p)

main :: IO ()
main = tictactoe1P
