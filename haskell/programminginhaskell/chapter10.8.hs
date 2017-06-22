
import Control.Concurrent (threadDelay)


type Pos = (Int,Int)

type Board = [Pos]

width :: Int
width = 10

height :: Int
height = 10

--

glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

b1 :: Board
b1 = [(1, 1), (2, 1), (2, 2), (2, 3), (3, 3), (3, 4), (4, 3)]

b2 :: Board
b2 = [(3,4), (4, 4), (5, 4)]

--

main :: IO ()
main = life b2 0

life :: Board -> Int -> IO ()
life board gen = do cls
                    showcells board
                    goto (0, height + 1)
                    putStrLn ("\nGeneration " ++ show gen)
                    threadDelay 100000
                    life (nextgen board) (gen + 1)

cls :: IO ()
cls = putStr "\ESC[2J"

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

showcells :: Board -> IO ()
showcells board = sequence_ [writeat p "O" | p <- board]

--wait :: Int -> IO ()
--wait n = sequence_ [return () | _ <- [1..(n * 1000000)]]

--

nextgen :: Board -> Board
nextgen board = survivors board ++ births board

isAlive :: Board -> Pos -> Bool
isAlive board pos = elem pos board

isEmpty :: Board -> Pos -> Bool
isEmpty board pos = not (isAlive board pos)

wrap :: Pos -> Pos
wrap (x,y) = (((x - 1) `mod` width) + 1, ((y - 1) `mod` height) + 1)

neighbours :: Pos -> [Pos]
neighbours (x,y) = map wrap [(x + x', y + y') | x' <- [-1..1], y' <- [-1..1],
                             x' /= 0 || y' /= 0]

liveneighbours :: Board -> Pos -> Int
liveneighbours board = length . filter (isAlive board) . neighbours

survivors :: Board -> [Pos]
survivors board = [p | p <- board, elem (liveneighbours board p) [2,3]]

births :: Board -> [Pos]
births board = [(x,y) | x <- [1..width], y <- [1..height],
                isEmpty board (x,y),
                liveneighbours board (x,y) == 3]

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)
