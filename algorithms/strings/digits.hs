digits :: String -> String
digits [] = []
digits (a:as) | ((a<='9')&&(a>='0')) = a:digits as
              | otherwise = digits as