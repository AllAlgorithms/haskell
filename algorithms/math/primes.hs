-- Prime Number Functions
-- Author : Satyajit Ghana (satyajit_ghana)

-- primes : generates a list of all the prime numbers
primes :: [Integer]
primes = filterPrime [2..] 
  where filterPrime (p:xs) = 
          p : filterPrime [x | x <- xs, x `mod` p /= 0]

main :: IO()
main = do
    putStrLn "Showing first 100 primes : "
    print (take 100 primes)
    putStrLn ("The 10th prime : " ++ show (primes !! 9))