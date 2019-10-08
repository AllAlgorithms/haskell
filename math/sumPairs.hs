sumPairs :: [(Int,Int)] -> [Int]
sumPairs [] = []
sumPairs ((a,b):as) = (a+b):sumPairs(as)