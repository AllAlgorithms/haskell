module Xor
    ( encodeB
    , decodeB
    , encode
    , decode
    ) where

import Data.Char
import Data.Bits

-- Raw encoding function for lists of numbers
-- First argument is the key, second is the data
-- The key will be repeated to the length of the data
encodeB :: Bits a => [a] -> [a] -> [a]
encodeB key = zipWith xor (cycle key)

-- Decoding is the same as encoding
decodeB :: Bits a => [a] -> [a] -> [a]
decodeB = encodeB

-- To encode a String, we first map each character to a number,
-- then map the numbers back to characters after encoding
encode :: String -> String -> String
encode key txt = map chr $ encodeB (map ord key) (map ord txt)

-- Decoding is the same as encoding
decode :: String -> String -> String
decode = encode
