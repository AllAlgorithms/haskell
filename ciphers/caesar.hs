module Caesar
    ( encode,
      crack
    ) where
{-| Using example:
 -| encode 5 "It's secret"
 -| "ny'x xjhwjy"
 -| encode (-5) "ny'x xjhwjy"
 -| "it's secret"
 -| crack "ny'x xjhwjy"
 -| "it's secret"
 -|
 -| Idea for crack is from:
 -| Hutton, Graham. Programming in Haskell. Cambridge University Press.
 -}

import Data.List
import Data.Char

count :: Char -> String -> Int
count x = length . filter (==x)

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
    where n = length xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack = crack' . lower

crack' :: String -> String
crack' xs = encode (-factor) . lower $ xs
    where (Just factor) = minimum chitab `elemIndex` chitab
          chitab = [chisqr (rotate n table') table | n <- [0..25]]
          table' = freqs xs

abc = ['a'..'z']

encodeChar n c = case c `elemIndex` abc of
               Just k -> abc !! ((n + k) `mod` length abc)
               _ -> c

lower :: String -> String
lower = map toLower

encode n = map (encodeChar n) . lower
