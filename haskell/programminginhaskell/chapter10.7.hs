import Data.Char

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r stars | (r,stars) <- zip [1..] board]
    where update r stars
            | r == row  = stars - num
            | otherwise = stars

main :: IO ()
main = play initial 1

play :: Board -> Int -> IO ()
play board player =
    do putBoard board
       if finished board then
           do putStr "\nPlayer "
              putStr (show (next player))
              putStrLn " wins!"
       else
           do putStr "\nPlayer "
              putStrLn (show player)
              row <- getDigit "Enter a row number:"
              num <- getDigit "Stars to remove:"
              if valid board row num then
                  play (move board row num) (next player)
              else
                  do putStrLn "\nERROR: Invalid move."
                     play board player 

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putStrLn "\nBoard"
                          putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

getDigit :: String -> IO Int
getDigit prompt = do putStrLn prompt
                     x <- getChar
                     getLine
                     if isDigit x then
                         return (digitToInt x)
                     else
                         do putStrLn "ERROR: Invalid digit."
                            getDigit prompt
