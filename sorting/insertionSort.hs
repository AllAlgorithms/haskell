insert :: Ord a => a -> [a] -> [a]
insert elem [] = [elem]
insert elem (x:[]) = min elem x:max elem x:[]
insert elem (x:y:xs)
  | elem > x && elem < y = x:elem:y:xs
  | elem < x && elem < y = elem:x:y:xs 
  | otherwise            = x:insert elem (y:xs)


insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) =
  foldl (\acc a -> insert a acc) (insert x []) xs
