subsets :: [a] -> [[a]]
subsets []       = [[]]
subsets (x : xs) = subsets xs ++ map (x :) (subsets xs)

-- subsets [1,2,3]
-- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]