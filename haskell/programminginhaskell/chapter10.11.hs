import System.IO

-- Exercise 1

putStr' :: String -> IO ()
putStr' s = sequence_ [putChar c | c <- s]

-- Exercise 2

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

putBoard :: Board -> IO ()
putBoard board = putBoardRow board 1

putBoardRow :: Board -> Int -> IO ()
putBoardRow [] _       = return ()
putBoardRow (b:bs) row = do putRow row b
                            putBoardRow bs (row + 1)

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

-- Exercise 3

putBoard' :: Board -> IO ()
putBoard' board = sequence_ [putRow num row | (num,row) <- zip [1..] board]

-- Exercise 4

adder :: IO ()
adder = do putStr "How many numbers? "
           line <- getLine
           let count = read line :: Int
           total <- addnumbers count 0
           putStrLn ("The total is " ++ show total)

addnumbers :: Int -> Int -> IO Int
addnumbers count acc = do if count <= 0 then
                              do return acc
                          else
                              do line <- getLine
                                 let number = read line :: Int
                                 addnumbers (count - 1) (acc + number)

-- Exercise 5

adder' :: IO ()
adder' = do putStr "How many numbers? "
            line <- getLine
            let count = read line :: Int
            numbers <- sequence (take count (repeat getLine))
            let total = sum (map read numbers)
            putStrLn ("The total is " ++ show total)

-- Exercise 6

sgetChar :: IO Char
sgetChar = do hSetEcho stdin False
              x <- getChar
              hSetEcho stdin True
              return x

readLine :: IO String
readLine = do line <- readLineReversed []
              return (reverse line)

readLineReversed :: String -> IO String
readLineReversed acc = do x <- sgetChar
                          case x of
                              '\n'      -> do putChar x
                                              return acc
                              '\DEL'    -> do if acc == [] then
                                                  do readLineReversed []
                                              else
                                                  do putChar '\b'
                                                     putChar ' '
                                                     putChar '\b'
                                                     readLineReversed (tail acc)
                              otherwise -> do putChar x
                                              readLineReversed (x:acc)