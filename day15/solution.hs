import Data.Maybe (mapMaybe, fromJust)
import Data.List (sortBy, find)
import Debug.Trace (trace)

type Point = (Int, Int)
type Range = (Int, Int)

data Sensor = Sensor { position :: Point, beacon :: Point } deriving Show

parseSensor :: String -> Sensor
parseSensor line =
  Sensor (positionX, positionY) (beaconX, beaconY)
  where
    cmp = words line
    positionX = read . init . drop 2 $ cmp!!2
    positionY = read . init . drop 2 $ cmp!!3
    beaconX = read . init . drop 2 $ cmp!!8
    beaconY = read . drop 2 $ cmp!!9

distanceBetween :: Point -> Point -> Int
distanceBetween (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

rangeAtRow :: Int -> Sensor -> Maybe Range
rangeAtRow row (Sensor position@(sensorX, sensorY) beacon)
  | leftX <= rightX = Just (leftX, rightX)
  | otherwise = Nothing
  where
    distance = distanceBetween position beacon
    leftX = sensorX - distance + shift
    rightX = sensorX + distance - shift
    shift = abs (row - sensorY)

mergeRanges :: Range -> Range -> [Range]
mergeRanges (l1, r1) (l2, r2)
  | l1 == l2 || r1 > l2 = [(l1, max r1 r2)]
  | l2 - r1 <= 1 = [(l1, r2)]
  | otherwise = [(l1, r1), (l2, r2)]

combineRanges :: [Range] -> [Range]
combineRanges = foldl combineTwo []
  where
    combineTwo acc range
      | null acc = [range]
      | otherwise = init acc ++ mergeRanges (last acc) range


buildRanges :: Int -> [Sensor] -> [Range]
buildRanges y = combineRanges . sortBy (\range1 range2 -> compare (fst range1) (fst range2)) . mapMaybe (rangeAtRow y)

solve1 :: Int -> [Sensor] -> Int
solve1 y = sum . map (\(l, r) -> r - l) . buildRanges y

solve2 :: Int -> [Sensor] -> Int
solve2 maxRow sensors =
  tuningFreq (snd (head ranges) + 1, y)
  where
    (ranges, y) = fromJust $ find (\(ranges, y) -> length ranges > 1) $ map (\y -> (buildRanges y sensors, y)) [0..maxRow]
    tuningFreq (x, y) = x * 4000000 + y

main = do
  content <- readFile "input.txt"
  let sensors = map parseSensor . lines $ content

  print $ "Solution 1 is " ++ show (solve1 2000000 sensors)
  print $ "Solution 2 is " ++ show (solve2 4000000 sensors)


