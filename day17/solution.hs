{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Debug.Trace (trace)

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

simulateRockFall :: [Char] -> Chamber -> Rock -> ([Char], Chamber)
simulateRockFall (jet:restJets) chamber rock =
  case maybeFallenRock of
    Just newRock -> simulateRockFall restJets chamber newRock
    Nothing -> (restJets, foldl (flip S.insert) chamber rockAfterJet)
  where
    rockAfterJet = handleJet jet chamber rock
    maybeFallenRock = goDown chamber rockAfterJet

throwRocks :: Int -> [RockType] -> [Char] -> Chamber -> Chamber
throwRocks 0 _ jets chamber = chamber
throwRocks n (rockType:rockTypes) jets chamber =
  throwRocks (n - 1) rockTypes newJets newChamber
  where
    rock = findStartPosition rockType chamber
    (newJets, newChamber) = simulateRockFall jets chamber rock

solve1 :: [RockType] -> [Char] -> Chamber -> Int
solve1 rockTypes jets chamber = maximum . map snd . S.elems $ throwRocks 2022 rockTypes jets chamber

-- throwRocks2 :: Int -> [(Int, RockType)] -> [(Int, Char)] -> Chamber -> Chamber
-- throwRocks2 n (rockType:rockTypes) jets chamber =
--   throwRocks2 (n + 1) rockTypes (map snd newJets) newChamber
--   where
--     rock = findStartPosition rockType chamber
--     (newJets, newChamber) = simulateRockFall jets chamber rock

drawChamber chamber = do
  mapM_ (putStrLn . row) $ reverse [1..maxY chamber + 8]
  putStrLn $ replicate 10 '-'
  where
    row y = "|" ++ map (\x -> symbolAt (x, y)) [0..7] ++ "|"

    symbolAt point
      | point `elem` chamber = '#'
      | otherwise = '.'

main = do
  content <- readFile "input.txt"

  let jets = concat . repeat . head . lines $ content
  let rockTypes = concat . repeat $ [HorizontalLine, Cross, MirroredL, VerticalLine, Square]
  let chamber = S.empty

  -- print $ "Solution 1 is " ++ show (solve1 rockTypes jets chamber)

  -- let jetsLength = length (head . lines $ content)
  -- let rockTypesLength = 4 * 5
  -- let n = lcm rockTypesLength jetsLength
  -- let rockCount = n `div` 4
  -- print $ n
  -- print $ rockCount
  -- print $ 1000000000000 `mod` n
  -- print $ (1000000000000 `div` rockCount)
  print $ maximum $ map snd $ S.elems $ throwRocks 10 rockTypes jets chamber
  -- print $ maximum $ map snd $ S.elems $ throwRocks 82 rockTypes jets chamber
