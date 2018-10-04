fact :: (Integral a) => a -> a
fact 0 = 1
fact n = n * fact (n - 1)

fact' :: (Integral a) => a -> a -> a
fact' 0 acc = acc
fact' n acc = fact' (n - 1) (n * acc)
