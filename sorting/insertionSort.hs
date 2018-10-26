insert :: Ord a => a -> [a] -> [a]
insert elem [] = [elem]
insert elem (x:[]) = min elem x:max elem x:[]
insert elem (x:y:xs) =
  if elem > x && elem < y
    then x:elem:y:xs
    else if elem < x && elem < y
    then elem:x:y:xs
    else x:insert elem (y:xs)


insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) =
  foldl (\acc a -> insert a acc) (insert x []) xs
