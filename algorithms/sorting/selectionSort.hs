select :: Ord a => [a] -> (a,[a])
-- Returns least element of list, and the rest
select [x] = (x,[])
select (x:xs) =
  let (m,rest) = select xs in
    case compare x m of
        LT -> (x,xs)
        _  -> (m,x:rest)

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs@(_:_) =
  let (m,rest) = select xs in
    m:(selectionSort rest)
