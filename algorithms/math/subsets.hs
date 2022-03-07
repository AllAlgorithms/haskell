import Control.Monad  -- for filterM

subsets :: [a] -> [[a]]
subsets []       = [[]]
subsets (x : xs) = subsets xs ++ map (x :) (subsets xs)

-- subsets [1,2,3]
-- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]

subsets' :: [a] -> [[a]]
subsets' = filterM (const [False, True])

-- subsets' [1,2,3]
-- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]

