module Vigenere
    ( encode
    , decode
    ) where

{-
 - Usage example:
 - > encode "KEY" "I love Haskell"
 - "S pmfi Fkwiopj"
 - > decode "KEY" "S pmfi Fkwiopj"
 - "I love Haskell"
 -}

import Data.Char
import Data.List

-- Take an arbitrary string and turn it into an infinitely cycling key
-- Non-alphabetic characters are ignored completely
-- Each alphabetic character is mapped to its index in the alphabet, case-insensitive
gen_key :: String -> [Int]
gen_key = cycle
    . map (\x -> ord x - ord 'A')
    . filter isAsciiUpper
    . map toUpper

-- Generalized version of encoding/decoding
-- Takes an operation which determines how to apply the shift to a given character (+ for encoding, - for decoding)
-- Second argument is the key, third argument is the data to encode/decode
-- The behavior of this encoder/decoder is designed to be identical to dcode.fr's implementation:
-- * Non-alphabetic characters are preserved, and the key is not advanced when going through non-alphabetic characters
-- * Both uppercase and lowercase letters are mapped, with case preserved
-- If the key is empty, the original string is returned with no modifications
-- Encoding and decoding with the same key should always be inverse operations
gencode :: (Int -> Int -> Int) -> String -> String -> String
gencode _ "" = id
gencode op key =
    reverse -- The below process leaves our string in reverse since each character is prepended to the rest
    . fst -- Once the process is done we don't care about the key, just take the result
    . foldl' enc_chr ("", (gen_key key)) -- Fold over the data; the accumulator is a tuple of the (reversed) current output and remaining key
    where
    -- enc_chr encodes/decodes one character, prepends it to the result accumulator, and advances the key if necessary
    enc_chr (done, shfts@(shft:rest)) c
        -- If the character is an ASCII letter, we shift it
        | isAsciiLower c = (do_shft shft c 'a' : done, rest)
        | isAsciiUpper c = (do_shft shft c 'A' : done, rest)
        -- Otherwise, just prepend the original character to the result accumulator, and don't advance the key
        | otherwise = (c : done, shfts)
    -- Perform the shift, using the specified operation
    -- Since the operation is done mod 26, any binary function on Int should produce a valid result
    do_shft shft c base = chr ((((ord c - ord base) `op` shft) `mod` 26) + ord base)

encode :: String -> String -> String
encode = gencode (+)

decode :: String -> String -> String
decode = gencode (-)
