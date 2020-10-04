anagram :: String -> String -> Bool
anagram [] [] = True
anagram a [] = False
anagram [] a = False
anagram (a:as) (b:bs) = if a==b then anagram as bs else False