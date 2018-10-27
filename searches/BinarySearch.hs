binarySearch :: (Ord a) => [a] -> a -> Maybe Int
binarySearch haystack needle = binarySearch' haystack needle 0 (length haystack - 1)
    where
        binarySearch' ls value low high
            | high < low = Nothing
            | pivot > value = binarySearch' ls value low (mid - 1)
            | pivot < value = binarySearch' ls value (mid + 1) high
            | otherwise = Just mid
            where
                mid = low + ((high - low) `div` 2)
                pivot = ls !! mid
