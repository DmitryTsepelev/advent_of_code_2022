{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

import qualified Data.Map as M
import Data.List (find, sort, sortBy)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

type Graph = M.Map String (Int, [String])
type GraphState = M.Map String Bool

parseGraph :: [String] -> Graph
parseGraph = M.fromList . map parseLine

parseLine :: [Char] -> ([Char], (Int, [String]))
parseLine line =
  (name, (rate, tunnels))
  where
    name = take 2 . drop 6 $ line
    rate = read . takeWhile (/= ';') . drop 23 $ line
    tunnels = map (filter (/= ',')) . words . drop 24 . dropWhile (/= ';') $ line

-- solve :: String -> Graph -> GraphState -> Int -> Int -> Int
-- solve _ _ _ 0 rate = rate
-- solve current graph initialState time rate

-- openValve tunnel = map (\(name, current) -> (name, (name == tunnel) || current))
openValve tunnel = M.insert tunnel True

rateAt :: String -> Graph -> Int
rateAt tunnel graph = fst $ graph M.! tunnel

-- valveIsOpen :: String -> GraphState -> Bool
-- valveIsOpen tunnel = snd . fromJust . find (\(name, _) -> name == tunnel)
valveIsOpen :: GraphState -> String -> Bool
valveIsOpen = (M.!)

makeStep :: Graph -> (GraphState, String, Int, Int, Int) -> [(GraphState, String, Int, Int, Int)]
makeStep graph (state, current, rate, ratePerMinute, 0) = [(state, current, rate, ratePerMinute, 0)]
makeStep graph (state, current, rate, ratePerMinute, time)
  | all (== True) . M.elems $ state = [(state, current, rate + ratePerMinute * time, ratePerMinute, 0)]
  | otherwise = concatMap (makeStep graph) $ concatMap processTunnel tunnels
  -- | otherwise = concatMap (makeStep graph) $ concatMap processTunnel tunnels
  where
    tunnels = snd $ graph M.! current

    processTunnel tunnel =
      if time >= 2 && not (valveIsOpen state tunnel) then
        [
          (state, tunnel, rate + ratePerMinute, ratePerMinute, time - 1),
          (openValve tunnel state, tunnel, rate + ratePerMinute * 2, ratePerMinute + rateAt tunnel graph, time - 2)
        ]
      else
        [
          (state, tunnel, rate + ratePerMinute, ratePerMinute, time - 1)
        ]

    takeTop time percent states = do
      if time <= 20 then do
        let all = sortBy (flip (\r1 r2 -> compare (getRate r1) (getRate r2))) $ states
        take ((length all * percent) `div` 100) all
      else
        states

getRate (_, _, rate, _, _) = rate

main = do
  content <- readFile "input.txt"
  let graph = parseGraph . lines $ content
  -- print graph
  let initialState = M.fromList . map (\key -> (key, fst (graph M.! key) == 0)) . M.keys $ graph
  -- let initialState = map (\(name, (rate, tunnels)) -> (name, rate == 0)) graph

  -- print $ graph
  -- print "======="

  -- print $ sort $ map (\(_, _, rate, ratePerMinute, _) -> (rate, ratePerMinute)) $ makeStep graph (initialState, "AA", 0, 0, 4)
  -- print $ map (\(state, _, rate, ratePerMinute, _) -> (map fst . filter ((== True) . snd) $ state, rate, ratePerMinute)) $ sort $ filter (\(_, _, rate, ratePerMinute, _) -> ratePerMinute == 33) $ makeStep graph (initialState, "AA", 0, 0, 8)
  -- print $ sort $ makeStep graph (initialState, "AA", 0, 0, 12, ["AA"])
  print $ last $ sort $ makeStep graph (initialState, "AA", 0, 0, 30)
