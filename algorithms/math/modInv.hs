-- |Modular inverse.
--
-- b = a^(-1) mod n
-- b * a - k * n = 1
--
-- Result only makes sense when (1 = gcd a n).
modInv :: Integer -> Integer -> Integer
modInv a n = let (_, b, _) = exgcd a n in b `mod` n

-- |Extended Euclidean algorithm.
exgcd :: Integer -> Integer -> (Integer, Integer, Integer)
exgcd a 0 = (a, 1, 0)
exgcd a b = (g, y, x - (a `div` b) * y)
  where (g, x, y) = exgcd b (a `mod` b)
