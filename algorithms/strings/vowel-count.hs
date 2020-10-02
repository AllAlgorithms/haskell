import Data.Char (toLower)

vowelCount :: String -> Int
vowelCount "" = 0
vowelCount (x:xs) = (if ((toLower x) `elem` ['a', 'e', 'i', 'o', 'u']) then 1 else 0) + vowelCount xs
