-- Collatz sequence
-- Author: <github.com/pablotrinidad>

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
    | even n = n:collatz (n `div` 2)
    | odd n = n:collatz (n*3 + 1)


-- Author: <github.com/monikanana>
-- function to count how long is callatz sequences with starting point = n
collatzLength :: Int -> Int
collatzLength n = length (collatz n)