import Data.List

isAnagram :: String -> String -> Bool
isAnagram str1 str2 = sort str1 == sort str2
