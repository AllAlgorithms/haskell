insert :: Ord a => a -> [a] -> [a]
insert e [] = [e]
insert e xs@(x:xs') =
  case compare e x of
    GT -> x : insert e xs'
    _  -> e : xs

insertionSort :: Ord a => [a] -> [a]
insertionSort =
  foldr insert []
