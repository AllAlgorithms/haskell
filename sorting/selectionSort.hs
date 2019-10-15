import Data.List

selectionSort :: Ord a => [a] -> [a]
selectionSort [x] = [x]
selectionSort xs = let minValue = minimum xs 
                    in minValue : selectionSort (delete minValue xs)