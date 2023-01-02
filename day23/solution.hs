import Control.Monad.State
import Data.List (find)
import Data.Maybe (maybe)
import qualified Data.Set as S
import qualified Data.Map as M

type Point = (Int, Int)
type Elves = S.Set Point
type Proposals = M.Map Point Point

-- Parsing

parseField :: [String] -> Elves
parseField lines =
  S.fromList $ foldl (\acc (row, y) -> acc ++ parseRow y row) [] (zip lines [1..])

parseRow :: Int -> String -> [Point]
parseRow y row = foldl (\acc (c, x) -> if c == '#' then (x, y):acc else acc) [] $ zip row [1..]

-- Solutions

solve1 :: Int -> Elves -> Int
solve1 turns elves = coveredGround $ solve1' 0 turns elves

coveredGround :: Elves -> Int
coveredGround elves = (width * height) - elvesCount
  where
    width = maximum xPositions - minimum xPositions + 1
    xPositions = map fst $ S.elems elves

    height = maximum yPositions - minimum yPositions + 1
    yPositions = map snd $ S.elems elves

    elvesCount = S.size elves

solve1' currentTurn turns elves
  | turns == currentTurn = elves
  | otherwise = solve1' (succ currentTurn) turns (evalState (makeTurn currentTurn) elves)

solve2 :: Elves -> Int
solve2 = succ . solve2' 0

solve2' currentTurn elves = do
  let newState = evalState (makeTurn currentTurn) elves
  if elves == newState then
    currentTurn
  else
    solve2' (succ currentTurn) newState

-- Simulation

makeTurn :: Int -> State Elves Elves
makeTurn turn = do
  elves <- get
  let proposals = fst . foldl (addProposals (propose elves turn)) (M.empty, S.empty) $ elves
  return $ foldl (moveElf proposals) elves (M.keys proposals)

moveElf :: Proposals -> Elves -> Point -> Elves
moveElf proposals elves proposal = S.insert proposal (S.delete (proposals M.! proposal) elves)

addProposals :: (Point -> Maybe Point) -> (Proposals, Elves) -> Point -> (Proposals, Elves)
addProposals propose plan@(proposals, stuckElves) elf =
  maybe plan tryAddProposal (propose elf)
  where
    tryAddProposal proposal
      | isStuckProposal proposal = plan
      | alreadyPlannedPoint proposal = (M.delete proposal proposals, S.insert elf stuckElves)
      | otherwise = (M.union proposals (M.singleton proposal elf), stuckElves)

    isStuckProposal point = S.member point stuckElves
    alreadyPlannedPoint point = M.member point proposals

directionN = (0, -1)
directionNE = (1, -1)
directionNW = (-1, -1)
directionS = (0, 1)
directionSE = (1, 1)
directionSW = (-1, 1)
directionW = (-1, 0)
directionE = (1, 0)

directions turn = take 4 . drop turn . cycle $ [
    (directionN, [directionNE, directionN, directionNW]),
    (directionS, [directionSE, directionS, directionSW]),
    (directionW, [directionNW, directionW, directionSW]),
    (directionE, [directionNE, directionE, directionSE])
  ]

propose :: Elves -> Int -> Point -> Maybe Point
propose elves turn point@(x, y)
  | hasNeighbour = buildProposal point <$> tryFindDirection
  | otherwise = Nothing
  where
    hasNeighbour = any (\(dx, dy) -> S.member (x + dx, y + dy) elves) allDirections
    tryFindDirection = find (\(direction, positions) -> canMove point positions) (directions turn)
    canMove (x, y) = not . any (\(dx, dy) -> S.member (x + dx, y + dy) elves)
    buildProposal (x, y) ((dx, dy), _) = (x + dx, y + dy)
    allDirections = [directionN, directionNE, directionNW, directionS, directionSE, directionSW, directionW, directionE]

main = do
  content <- readFile "input.txt"
  let elves = parseField (lines content)
  print $ "Solution 1 is " ++ show (solve1 10 elves)
  print $ "Solution 2 is " ++ show (solve2 elves)
