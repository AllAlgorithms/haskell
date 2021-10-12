-- Extended Euclidean algorithm.
-- let (g, x, y) = exgcd a b
-- g = x * a + y * b
exgcd :: Integer -> Integer -> (Integer, Integer, Integer)
exgcd a 0 = (a, 1, 0)
exgcd a b = (g, y, x - (a `div` b) * y)
  where (g, x, y) = exgcd b (a `mod` b)
