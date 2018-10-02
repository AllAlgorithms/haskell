
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

list :: [Int]
list = [factorial x | x <- [1..] ]

getNFactorial :: Int -> [Int]
getNFactorial n = take n list
