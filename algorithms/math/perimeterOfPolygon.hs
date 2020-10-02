type Point = (Int, Int)

euclideanDist :: Point -> Point -> Double
euclideanDist (x1, y1) (x2, y2) =
  sqrt . fromIntegral $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2


perimeter :: [Point] -> Double
perimeter []           = 0
perimeter (start : xs) = perimeter_ (start : xs)
 where
  perimeter_ []           = 0
  perimeter_ [x         ] = 0
  perimeter_ (x : y : []) = euclideanDist x y + euclideanDist y start
  perimeter_ (x : y : xs) = euclideanDist x y + perimeter_ (y : xs)

-- perimeter [(0,0), (1,0), (1,1), (0,1)]
-- 4.0