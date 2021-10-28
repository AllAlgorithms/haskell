module Xor
    ( encodeB
    , decodeB
    , encode
    , decode
    ) where

import Data.Char
import Data.Bits

encodeB :: Bits a => [a] -> [a] -> [a]
encodeB key = zipWith xor (cycle key)

decodeB :: Bits a => [a] -> [a] -> [a]
decodeB = encodeB

encode :: String -> String -> String
encode key txt = map chr $ encodeB (map ord key) (map ord txt)

decode :: String -> String -> String
decode = encode
