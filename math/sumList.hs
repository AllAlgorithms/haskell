--Faz o somatorio dos elementos da lista
sumList :: [Int] -> Int
sumList [] = 0
sumList (a:as) = a + sumList as