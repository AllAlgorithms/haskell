member :: [Int] -> Int -> Bool
member [] z = False
member (a:as) z | (a /= z) = (False || (member as z))
                | otherwise = True