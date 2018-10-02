quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort [a] = [a]
quickSort (x:xs) = smaller ++ [x] ++ bigger where
    bigger = quickSort $ filter (>=x) xs
    smaller = quickSort $ filter (<x) xs
