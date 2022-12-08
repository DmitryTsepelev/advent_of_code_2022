import Data.List (transpose, intersect, sort)
import Data.Char (digitToInt)
import Data.Set (toList, fromList)

calculateVisibility :: Int -> [[Int]] -> [[((Int, Int), Bool)]]
calculateVisibility gridSize input =
  map (\y -> map (\x -> ((y, x), visibleAt x y)) indexes) indexes
  where
    indexes = [0..gridSize]
    visibleAt x y = all (< (input!!y)!!x) (take x (input!!y))

calculateScenicScore :: Int -> [[Int]] -> [[((Int, Int), Int)]]
calculateScenicScore gridSize input =
  map (\y -> map (\x -> ((y, x), score x y)) indexes) indexes
  where
    indexes = [0..gridSize]
    score x y = do
      let result = length . takeWhile (< (input!!y)!!x) . reverse . take x $ (input!!y)
      if x > result then succ result else result

type CalculationResult a = [[((Int, Int), a)]]

transposeCoordinates :: CalculationResult a -> CalculationResult a
transposeCoordinates = map (map (\((x, y), visible) -> ((y, x), visible)))

performCalculations :: (Int -> [[Int]] -> CalculationResult a) -> [[Int]] -> CalculationResult a
performCalculations calculate input = map concat [leftToRight, rightToLeft, topToBottom, bottomToTop]
  where
    gridSize = pred . length $ input
    undoReverse = map (map (\((y, x), visible) -> ((y, gridSize - x), visible)))
    undoTranspose = transpose . transposeCoordinates

    leftToRight = calculate gridSize $ input
    topToBottom = undoTranspose . calculate gridSize . transpose $ input

    rightToLeft = undoReverse . calculate gridSize . map reverse $ input
    bottomToTop = undoTranspose . undoReverse . calculate gridSize . transpose . reverse $ input

countVisibleTrees :: [[Int]] -> Int
countVisibleTrees =
  length .
  toList .
  fromList .
  concatMap (map fst . filter snd) .
  performCalculations calculateVisibility

findBestScenicScore :: [[Int]] -> Int
findBestScenicScore =
  maximum .
  map product .
  transpose .
  map (map snd . sort) .
  performCalculations calculateScenicScore

main = do
  content <- readFile "input.txt"
  let input = map (map digitToInt) . lines $ content

  print $ "Solution 1 is " ++ show (countVisibleTrees input)
  print $ "Solution 2 is " ++ show (findBestScenicScore input)
