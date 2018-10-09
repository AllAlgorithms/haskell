fibonacci :: (Integral a) => a -> [a]
fibonacci 1 = [1]
fibonacci 2 = [1, 1]
fibonacci x
    | x < 1  = error "Fibonacci sequence length must be >= 1"
    | otherwise = n:fibs
        where
            fibs = fibonacci (x - 1)
            n = sum $ take 2 fibs

