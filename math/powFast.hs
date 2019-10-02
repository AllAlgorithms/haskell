sq :: (Num a) => a -> a
sq x  = x*x  

powFast :: (Integral a, Num p) => p -> a -> p
powFast base exp = if (exp == 0) then 1 else (sq $ powFast base (div exp 2)) * (verify exp base)
    where verify e b = if (mod e 2 == 0) then 1 else b
