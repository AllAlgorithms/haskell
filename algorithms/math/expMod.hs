-- Modular exponentiation
-- https://en.wikipedia.org/wiki/Modular_exponentiation

expMod :: Int -> Int -> Int -> Int
expMod b e m | e == 0    = 1
             | odd e     = b `mod` m * expMod b (e - 1) m `mod` m
             | otherwise = expMod ((b * b) `mod` m) (e `div` 2) m