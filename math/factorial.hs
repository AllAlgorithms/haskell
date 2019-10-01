factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Author: <github.com/monikanana>
-- factorial using folding
factorial_ :: Int -> Int
factorial_ n = foldl (*) 1 [1..n]