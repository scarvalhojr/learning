
primes :: [Int]
primes = primes' [2..]
  where primes' :: [Int] -> [Int]
        primes' (x:xs) = x : primes' (filter (notDivisibleBy x) xs)

notDivisibleBy :: Int -> Int -> Bool
notDivisibleBy x y = y `mod` x /= 0

main = putStrLn $ show (last (take 100000 primes))
