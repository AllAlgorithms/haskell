double :: [Int] -> [Int]
double [] = []
double (a:as) = (a*2):(double as)