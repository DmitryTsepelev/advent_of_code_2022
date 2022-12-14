{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Concurrent ( threadDelay )
import Data.List (sort, find)
import qualified Data.Set as S
import Debug.Trace (trace)
import Data.Maybe (isNothing, isJust, fromJust)

-- Parsing

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

parsePath :: String -> [Point]
parsePath =
  map listToPoint . filter isOdd . zip [1..] . words
  where
    listToPoint = (\[f, s] -> (read f, read s)) . (wordsWhen (== ',') . snd)
    isOdd (idx, _) = odd idx

pathToRock :: [Point] -> [Point]
pathToRock points =
  concatMap (\((x1, y1), (x2, y2)) -> [(x, y) | x <- rangeFrom x1 x2, y <- rangeFrom y1 y2]) pointPairs
  where
    pointPairs = zip points (tail points)
    rangeFrom p1 p2
      | p1 <= p2 = [p1..p2]
      | otherwise = rangeFrom p2 p1

-- Simulation

type Point = (Int, Int)
data FallResult s = Abyss s | Rest s | Block s deriving (Eq, Show)
type Floor = Maybe Int

throwSand :: Point -> Point -> S.Set Point -> Floor -> S.Set Point -> Int -> FallResult (S.Set Point)
throwSand startPoint (x, y) rocks maybeFloor sand maxY
  | isNothing maybeFloor && y > maxY = Abyss sand
  | Just point <- maybePointToFall = throwSand startPoint point rocks maybeFloor sand maxY
  | startPoint == (x, y) = Block $ S.insert (x, y) sand
  | otherwise = Rest $ S.insert (x, y) sand
  where
    maybePointToFall = find (\point -> point `S.notMember` rocks && point `S.notMember` sand && notOnFloor point) candidates
    notOnFloor (_, y)
      | (Just floorY) <- maybeFloor = floorY /= y
      | otherwise = True
    candidates = [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]

drawScreen :: Point -> S.Set Point -> Floor -> S.Set Point -> IO ()
drawScreen start rocks maybeFloor sand =
  mapM_ (putStrLn . row) [minY..maxY]
  where
    xValues = S.elems . S.map fst $ rocks
    minX = minimum xValues - 30
    maxX = maximum xValues + 30

    yValues = S.elems . S.map snd $ rocks
    minY = minimum yValues - 5
    maxY = maximum yValues + 5

    row y = map (\x -> symbolAt (x, y)) [minX..maxX]

    symbolAt point
      | point `elem` rocks = '#'
      | point `elem` sand = '.'
      | point == start = '+'
      | maybeFloor == Just (snd point) = '#'
      | otherwise = ' '

simulate :: Point -> Point -> S.Set Point -> Floor -> S.Set Point -> Int -> S.Set Point
simulate startPoint point rocks floor sand maxY
  | (Rest newSand) <- result = simulate startPoint point rocks floor newSand maxY
  | (Block newSand) <- result = newSand
  | otherwise = sand
  where
    result = throwSand startPoint point rocks floor sand maxY

clear :: IO ()
clear = putStr "\ESC[H"

simulateAndDraw :: Point -> Point -> S.Set Point -> Floor -> S.Set Point -> Int -> IO ()
simulateAndDraw startPoint point rocks floor sand maxY
  | (Rest newSand) <- result = do
    clear
    drawScreen startPoint rocks floor newSand
    threadDelay 5000
    simulateAndDraw startPoint point rocks floor newSand maxY
  | (Block newSand) <- result = return ()
  | otherwise = return ()
  where
    result = throwSand startPoint point rocks floor sand maxY


main = do
  content <- readFile "input.txt"
  let rocks = S.fromList . concatMap (pathToRock . parsePath) . lines $ content

  let maxY = maximum . S.elems . S.map snd $ rocks
  let solution1 = length $ simulate (500,0) (500,0) rocks Nothing S.empty maxY
  print $ "Solution 2 is " ++ show solution1

  let floor = Just $ maxY + 2
  let solution2 = length $ simulate (500,0) (500,0) rocks floor S.empty maxY
  print $ "Solution 2 is " ++ show solution2

  drawScreen (500,0) rocks floor (simulate (500,0) (500,0) rocks floor S.empty maxY)

  -- uncomment to visualize
  -- clear
  -- simulateAndDraw (500,0) (500,0) rocks floor S.empty maxY
