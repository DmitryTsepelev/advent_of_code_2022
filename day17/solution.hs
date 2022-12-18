{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Data.List (elemIndex)

type Point = (Int, Int)
type Chamber = S.Set Point

data RockType = HorizontalLine | Cross | MirroredL | VerticalLine | Square deriving (Eq, Show)
type Rock = [Point]

maxY :: Chamber -> Int
maxY chamber
  | S.null chamber = 0
  | otherwise = maximum $ S.map snd chamber

findStartPosition :: RockType -> Chamber -> Rock
findStartPosition rock chamber
  | rock == HorizontalLine = map (\xShift -> (xShift + 2, yShift + 4)) [0..3]
  | rock == Cross = [(2, yShift + 5), (3, yShift + 5), (4, yShift + 5), (3, yShift + 6), (3, yShift + 4)]
  | rock == MirroredL = [(2, yShift + 4), (3, yShift + 4), (4, yShift + 4), (4, yShift + 5), (4, yShift + 6)]
  | rock == VerticalLine = [(2, yShift + 4), (2, yShift + 5), (2, yShift + 6), (2, yShift + 7)]
  | rock == Square = [(2, yShift + 4), (2, yShift + 5), (3, yShift + 4), (3, yShift + 5)]
  where
    yShift = maxY chamber

move :: Point -> Rock -> Rock
move (dx, dy) = map (\(x, y) -> (x + dx, y + dy))

type Movement = Rock -> Rock

moveDown :: Movement
moveDown = move (0, -1)

moveRight :: Movement
moveRight = move (1, 0)

moveLeft :: Movement
moveLeft = move (-1, 0)

cannotMoveHorizontally :: Chamber -> Rock -> Bool
cannotMoveHorizontally chamber = any (\point@(x, y) -> point `elem` chamber || x < 0 || x > 6)

canGoDown :: Chamber -> Rock -> Bool
canGoDown chamber = all (\point@(x, y) -> point `notElem` chamber && y > 0)

handleJet :: Char -> Chamber -> Rock -> Rock
handleJet jet chamber rock
  | cannotMoveHorizontally chamber newRock = rock
  | otherwise = newRock
  where
    newRock = move rock
    move = if jet == '<' then moveLeft else moveRight

goDown :: Chamber -> Rock -> Maybe Rock
goDown chamber rock
  | canGoDown chamber newRock = Just newRock
  | otherwise = Nothing
  where newRock = moveDown rock

simulateRockFall :: [(Int, Char)] -> Chamber -> Rock -> ([(Int, Char)], Chamber)
simulateRockFall (jet:restJets) chamber rock =
  case maybeFallenRock of
    Just newRock -> simulateRockFall restJets chamber newRock
    Nothing -> (restJets, foldl (flip S.insert) chamber rockAfterJet)
  where
    rockAfterJet = handleJet (snd jet) chamber rock
    maybeFallenRock = goDown chamber rockAfterJet

throwRocks :: Int -> [(Int, RockType)] -> [(Int, Char)] -> Chamber -> Chamber
throwRocks 0 _ jets chamber = chamber
throwRocks n (rockType:rockTypes) jets chamber =
  throwRocks (n - 1) rockTypes newJets newChamber
  where
    rock = findStartPosition (snd rockType) chamber
    (newJets, newChamber) = simulateRockFall jets chamber rock

solve1 :: [(Int, RockType)] -> [(Int, Char)] -> Chamber -> Int
solve1 rockTypes jets chamber = maximum . map snd . S.elems $ throwRocks 2022 rockTypes jets chamber

-- findLoop :: Int -> S.Set (Int, Int) -> [(Int, RockType)] -> [(Int, Char)] -> Chamber -> (Int, S.Set (Int, Int))
findLoop n combinations (rockType:rockTypes) jets chamber
  | combination `notElem` combinations = findLoop (n + 1) (combinations ++ [combination]) rockTypes newJets newChamber
  | otherwise = (fromJust $ combination `elemIndex` combinations, n, combinations)
  where
    combination = (fst rockType, fst . head $ jets, getSurface chamber)

    getSurface points =
      map (\maximum -> maximum - minimum maximums) maximums
      where
        maximums = map (getMaximum points) [0..6]

    getMaximum points x =
      if null yValues then 0 else maximum yValues
      where yValues = map snd $ filter (\(current, _) -> current == x) $ S.elems points

    rock = findStartPosition (snd rockType) chamber
    (newJets, newChamber) = simulateRockFall jets chamber rock

drawChamber chamber = do
  mapM_ (putStrLn . row) $ reverse [1..maxY chamber + 8]
  putStrLn $ replicate 10 '-'
  where
    row y = "|" ++ map (\x -> symbolAt (x, y)) [0..7] ++ "|"

    symbolAt point
      | point `elem` chamber = '#'
      | otherwise = '.'

getHeight iterations rockTypes jets chamber =
  if null heights then 0 else maximum heights
  where heights = map snd $ S.elems $ throwRocks iterations rockTypes jets chamber

main = do
  content <- readFile "input.txt"

  let jets = concat . repeat . zip [0..] . head . lines $ content
  let rockTypes = concat . repeat $ zip [0..] [HorizontalLine, Cross, MirroredL, VerticalLine, Square]
  let chamber = S.empty

  -- print $ "Solution 1 is " ++ show (solve1 rockTypes jets chamber)

  let (cycleStart, cycleEnd, _) = findLoop 0 [] rockTypes jets chamber
  print $ (cycleStart, cycleEnd)

  let rocksCount = 1000000000000
  -- let rocksCount = 2022
  let heightAtCycleStart = getHeight cycleStart rockTypes jets chamber
  -- print heightAtCycleStart
  let heightAtCycleEnd = getHeight cycleEnd rockTypes jets chamber
  -- print heightAtCycleEnd
  -- 35
  -- print $ getHeight 98 rockTypes jets chamber
  -- print $ getHeight (98+35)  rockTypes jets chamber
  let lastIteration =  max 0 ((rocksCount - cycleStart - 1) `mod` cycleEnd)
  let lastIteration = 0
  print $ "lastIteration " ++ (show lastIteration)
  let lastUnfinishedCycleHeight = getHeight lastIteration rockTypes jets chamber
  print $ lastUnfinishedCycleHeight

  print $ getHeight (cycleStart - 1) rockTypes jets chamber + (heightAtCycleEnd - heightAtCycleStart) * (rocksCount `div` (cycleEnd - cycleStart)) + lastUnfinishedCycleHeight
