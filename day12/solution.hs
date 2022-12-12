import Data.Char (ord, isUpper)
import Data.List (elem, find, elemIndex, minimumBy, sortBy)
import Data.Maybe (isNothing, fromJust)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

type Field = M.Map Point Char
type Point = (Int, Int)
type Path = [Point]

parseField :: [[Char]] -> Field
parseField input =
  foldl parseRow M.empty [0..fieldHeight - 1]
  where
    parseRow field y = foldl (\field x -> M.insert (x, y) ((input!!y)!!x) field) field [0..fieldWidth - 1]
    fieldWidth = length (head input)
    fieldHeight = length input

findPosition :: [[Char]] -> Char -> (Int, Int)
findPosition field value =
  (x, y)
  where
    startLine = fromJust . find (\line -> value `elem` line) $ field
    x = fromJust . elemIndex value $ startLine
    y = fromJust . elemIndex startLine $ field

isValidPoint :: Field -> (Int, Int) -> (Int, Int) -> Bool
isValidPoint field (currentX, currentY) candidate@(x, y)
  | Just height <- maybeHeight = ord height - ord currentHeight <= 1
  | otherwise = False
  where
    maybeHeight = adjustStart <$> M.lookup (x, y) field
    adjustStart value = if value == 'E' then 'z' else value

    currentHeight = adjustEnd (field M.! (currentX,currentY))
    adjustEnd value = if value == 'S' then 'a' else value

validNextPoints point@(currentX, currentY) field =
  filter (isValidPoint field point) $ foldl (\acc (dx, dy) -> (currentX + dx, currentY + dy):acc) [] deltas
  where deltas = [(-1, 0), (1, 0), (0, -1), (0, 1)]

bfs :: S.Set (Int, Point) -> S.Set Point -> Field -> Int
bfs queue visited field
  | S.null queue = 0
  | field M.! toVisit == 'E' = steps
  | otherwise = bfs newPositionsToVisit (S.insert toVisit visited) field
    where
      (steps, toVisit) = S.elemAt 0 queue
      newPositionsToVisit = foldl (flip S.insert) (S.deleteAt 0 queue) $ map (\point -> (steps + 1, point)) (filter (not . flip S.member visited) $ validNextPoints toVisit field)

main = do
  content <- readFile "input.txt"
  let input = lines content

  let finish = findPosition input 'E'
  let queue = [[findPosition input 'S']]
  let field = parseField input

  print $ bfs (S.singleton (0, findPosition input 'S')) S.empty field

  let startPositions = S.fromList . map (\point -> (0, point)) . M.keys . M.filter (`elem` ['a', 'S']) $ field
  print $ bfs startPositions S.empty field
