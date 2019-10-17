-- Compute the hamming distance between two bytestrings
-- The shortest bytestring is padded with zeros at its beginning to have the same length as the other one
-- Example> hammingDistance [34, 128, 254, 2] [45, 67, 111]
-- 18

import qualified Data.Word as W
import Data.Bits

bitsFromWord :: W.Word8 -> [Int]
bitsFromWord word = [ if testBit word x then 1 else 0 | x <- [0..7]]

bytewiseHammingDistance :: W.Word8 -> W.Word8 -> Int
bytewiseHammingDistance bitsX bitsY = sum [ 1 | i <- [0..7], (bitsFromWord bitsX !! i) /= (bitsFromWord bitsY !! i)]

padByteString :: [W.Word8] -> Int -> [W.Word8]
padByteString byteString padLen = replicate padLen 0 ++ byteString

prepareByteStrings :: [W.Word8] -> [W.Word8] -> [(W.Word8, W.Word8)]
prepareByteStrings byteStringX byteStringY = if length byteStringX < length byteStringY
    then zip (padByteString byteStringX (length byteStringY - length byteStringX)) byteStringY
    else zip byteStringX (padByteString byteStringY (length byteStringX - length byteStringY))

hammingDistance :: [W.Word8] -> [W.Word8] -> Int
hammingDistance bytesX bytesY = sum [bytewiseHammingDistance x y | (x, y) <- prepareByteStrings bytesX bytesY]
