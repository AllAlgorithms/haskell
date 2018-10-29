-- https://www.cmi.ac.in/~madhavan/courses/prog2-2012/lectures/balanced-search-trees-in-haskell.txt

data Tree a = Empty | Node Int (Tree a) a (Tree a)

instance (Show a) => Show (Tree  a) where
  show = showTree

instance Functor Tree where
  -- fmap :: (a->b) -> Tree a -> Tree b
  fmap f Empty = Empty
  fmap f (Node h l v r) = Node h (fmap f l) (f v) (fmap f r)


showTree Empty            = ""
showTree n@(Node i _ _ _) = go i n
 where
  go _ Empty = ""
  go i (Node _ l c r) =
    go (i - 1) l
      ++ replicate (4 * fromIntegral i) ' '
      ++ show c
      ++ "\n"
      ++ go (i - 1) r

consTree :: Ord a => a -> Tree a -> Tree a
consTree x Empty = Node 1 Empty x Empty
consTree x t@(Node h l v r) | x < v  = rebalance (Node h (consTree x l) v r)
                            | x > v  = rebalance (Node h l v (consTree x r))
                            | x == v = t

rebalance :: Tree a -> Tree a
rebalance Empty = Empty
rebalance t@(Node h t1 y t2)
  | abs sy < 2           = updateHeight t
  | sy == 2 && st1 /= -1 = rotateright t
  | sy == 2 && st1 == -1 = rotateright (Node 0 (rotateleft t1) y t2)
  | sy == -2 && st2 /= 1 = rotateleft (Node 0 t1 y t2)
  | sy == -2 && st2 == 1 = rotateleft (Node 0 t1 y (rotateright t2))
 where
  sy  = slope t
  st1 = slope t1
  st2 = slope t2

  rotateright (Node _ (Node _ ll y lr) x r) =
    let r' = updateHeight (Node 0 lr x r) in updateHeight (Node 0 ll y r')

  rotateleft (Node _ l x (Node _ rl y rr)) =
    let l' = updateHeight (Node 0 l x rl) in updateHeight (Node 0 l' y rr)

slope :: (Tree a) -> Int
slope Empty            = 0
slope (Node _ t1 x t2) = (height t1) - (height t2)

updateHeight :: Tree a -> Tree a
updateHeight Empty            = Empty
updateHeight (Node _ l val r) = Node (max (height l) (height r) + 1) l val r

height :: (Tree a) -> Int
height Empty            = 0
height (Node m t1 x t2) = m

fromList :: Ord a => [a] -> Tree a
fromList = foldr consTree Empty


-- Usage
-- balancedTree = fromList [1..100]