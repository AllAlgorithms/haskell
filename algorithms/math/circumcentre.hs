module Circumcentre where

type Point2D a = (a, a)

circumcentre :: (Fractional a, Eq a) => Point2D a -> Point2D a -> Point2D a -> 
    Maybe (Point2D a)
circumcentre (x1, y1) (x2, y2) (x3, y3)
    | determinant == 0 = Nothing
    | otherwise = Just (x, y)
    where
        x = (cy2 * c1 - cy1 * c2) / determinant
        y = (cx1 * c2 - cx2 * c1) / determinant
        determinant = cx1 * cy2 - cx2 * cy1
        cx1 = 2 * (x3 - x1)
        cx2 = 2 * (x3 - x2)
        cy1 = 2 * (y3 - y1)
        cy2 = 2 * (y3 - y2)
        c1 = x3 * x3 - x1 * x1 + y3 * y3 - y1 * y1
        c2 = x3 * x3 - x2 * x2 + y3 * y3 - y2 * y2
