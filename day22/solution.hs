import Data.List (find, sort)
import qualified Data.Map as M
import Debug.Trace (trace)
import Data.Char (isDigit)

parseField lines =
  M.fromList $ foldl (\acc (row, y) -> acc ++ parseRow y row) [] (zip rows [1..])
  where
    rows = takeWhile (not . null) $ lines

parseCommands :: String -> [Command]
parseCommands "" = []
parseCommands s@(c:cs)
  | c == 'R' || c == 'L' = Rotate c : parseCommands cs
  | otherwise = do
    let digits = takeWhile isDigit s
    Move (read digits) : parseCommands (drop (length digits) s)

parseRow y row = foldl (\acc (c, x) -> ((x, y), c):acc) [] $ filter ((/= ' ') . fst) (zip row [1..])

findStartPosition :: Field -> Point
findStartPosition field = (x, 1)
  where x = minimum . map fst . M.keys . M.filterWithKey (\(_, y) kind -> kind == '.' && y == 1) $ field

data Direction = R | L | U | D deriving (Eq, Show, Read)

type Point = (Int, Int)
type Field = M.Map Point Char

data Command = Move Int | Rotate Char

go :: Field -> Point -> Int -> Direction -> Point
go _ position 0 _ = position
go field position@(x, y) steps direction
  | nextKind == '.' = go field nextPosition (pred steps) direction
  | otherwise = position
  where
    nextPosition = do
      let candidate = (x + dx, y + dy)
      if M.member candidate field then
        candidate
      else
        wrapAround field position direction

    (dx, dy) = deltas direction
    nextKind = field M.! nextPosition

deltas R = (1, 0)
deltas L = (-1, 0)
deltas U = (0, -1)
deltas D = (0, 1)

rotate :: Direction -> Char -> Direction
rotate direction rotation = read $ rotations M.! show direction
  where
    rotations = if rotation == 'R' then clockwise else counterclockwise
    clockwise = M.fromList [("R", "D"), ("D", "L"), ("L", "U"), ("U", "R")]
    counterclockwise = M.fromList [("R", "U"), ("D", "R"), ("L", "D"), ("U", "L")]

wrapAround :: Field -> Point -> Direction -> Point
wrapAround field (currentX, currentY) R = (minimum . map fst . M.keys . M.filterWithKey (\(_, y) kind -> y == currentY) $ field, currentY)
wrapAround field (currentX, currentY) L = (maximum . map fst . M.keys . M.filterWithKey (\(_, y) kind -> y == currentY) $ field, currentY)
wrapAround field (currentX, currentY) U = (currentX, maximum . map snd . M.keys . M.filterWithKey (\(x, _) kind -> x == currentX) $ field)
wrapAround field (currentX, currentY) D = (currentX, minimum . map snd . M.keys . M.filterWithKey (\(x, _) kind -> x == currentX) $ field)

simulate field position =
  foldl performCommand (position, R)
  where
    performCommand (position, direction) (Move steps) = (go field position steps direction, direction)
    performCommand (position, direction) (Rotate rotation) = (position, rotate direction rotation)

getPassword (x, y) direction =
  1000 * y + 4 * x + facing
  where
    facing =
      case direction of
        R -> 0
        D -> 1
        L -> 2
        U -> 3


main = do
  content <- readFile "input.txt"
  let field = parseField (lines content)
  let commands = parseCommands (last . lines $ content)
  let startPosition = findStartPosition field

  let (position, direction) = simulate field startPosition commands

  print $ getPassword position direction
