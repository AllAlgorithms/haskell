module ConwaysGameOfLife (
  GoL(..),
  Cell(..),
  conwayNGenerations
) where

data GoL = GoL (List2D Cell)

instance Show GoL where
  show (GoL cellRows) = unlines $ map concat $ map (map show) cellRows

data Cell = Dead | Live deriving (Eq, Ord)

instance Show Cell where
  show Dead = "⬜️"
  show Live = "🔳"

conwayNGenerations :: Int -> GoL -> [GoL]
conwayNGenerations n gol = scanl (\ gen _ -> nextGeneration gen) gol [1..(n - 1)]

nextGeneration :: GoL -> GoL
nextGeneration gol@(GoL cellRows) =
  let nextRow rowCoords = map (nextCell gol) rowCoords
  in GoL $ map nextRow $ coordinate cellRows

nextCell :: GoL -> Coords -> Cell
nextCell gol@(GoL cellRows) cellCoords =
  let cell = list2DLookupDefault cellRows Dead cellCoords
      liveNeighborCount = countLiveNeighbors gol cellCoords
  in nextCellState cell liveNeighborCount

nextCellState :: Cell -> Int -> Cell
nextCellState cell liveNeighborCount
  | (cell == Live) && (liveNeighborCount `elem` [2, 3]) = Live
  | (cell == Dead) && (liveNeighborCount == 3) = Live
  | otherwise = Dead

countLiveNeighbors :: GoL -> Coords -> Int
countLiveNeighbors (GoL list2D) (i, j) =
  let getCell = list2DLookupDefault list2D Dead
      neighbors = map getCell $ neighborCoords (i, j)
  in length $ filter ((==) Live) neighbors


type List2D a = [[a]]
type Coords = (Int, Int)

coordinate :: List2D a -> List2D Coords
coordinate [] = []
coordinate cellRows =
  let rowCount = length cellRows
      coordinateCols (row, rowIndex) =
        let colCount = length row
        in [(rowIndex, c) | c <- [0..(colCount - 1)]]
  in map coordinateCols (zip cellRows [0..(rowCount - 1)])

list2DLookupDefault :: List2D a -> a -> Coords -> a
list2DLookupDefault list2D d (i, j)
  | i < 0 || j < 0 = d
  | (length list2D) <= i = d
  | (length (list2D !! i)) <= j = d
  | otherwise = list2D !! i !! j

neighborCoords :: Coords -> [Coords]
neighborCoords (i, j) =
  let neighborDirections = [(di, dj) | let ds = [-1, 0, 1], di <- ds, dj <- ds, (di, dj) /= (0, 0)]
  in map (\ (di, dj) -> (i + di, j + dj)) neighborDirections
