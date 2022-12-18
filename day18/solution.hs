import qualified Data.Set as S

type Point = (Int, Int, Int)

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

parseCube :: [Char] -> Point
parseCube line = (read (head cmp), read (cmp!!1), read (cmp!!2))
  where cmp = wordsWhen (== ',') line

neighbors (x, y, z) = [(x + 1, y, z), (x - 1, y, z), (x, y + 1, z), (x, y - 1, z), (x, y, z + 1), (x, y, z - 1)]

getCorners lava =
  ((minX, minY, minZ), (maxX, maxY, maxZ))
  where
    x (x, _, _) = x
    y (_, y, _) = y
    z (_, _, z) = z
    minX = (minimum . map x $ lava) - 1
    minY = (minimum . map y $ lava) - 1
    minZ = (minimum . map z $ lava) - 1
    maxX = (maximum . map x $ lava) + 1
    maxY = (maximum . map y $ lava) + 1
    maxZ = (maximum . map z $ lava) + 1

canBeVisited (lx, ly, lz) (rx, ry, rz) visited point@(x, y, z) =
  point `S.notMember` visited &&
    x >= lx && x <= rx &&
    y >= ly && y <= ry  &&
    z >= lz && z <= rz

isLava lava point = point `elem` lava

fill :: Int -> (S.Set Point -> Point -> Bool) -> (Point -> Bool) -> S.Set Point -> S.Set Point -> Int
fill surface isValidPoint isLava visited queue
  | S.null queue = surface
  | otherwise = fill' surface isValidPoint isLava visited queue

fill' :: Int -> (S.Set Point -> Point -> Bool) -> (Point -> Bool) -> S.Set Point -> S.Set Point -> Int
fill' surface isValidPoint isLava visited queue = do
  fill newSurface isValidPoint isLava toVisit newQueue
  where
    allNeighbors = neighbors current
    notVisitedNeighbors = filter (isValidPoint visited) $ neighbors current
    current = S.elemAt 0 queue
    newSurface = surface + length (filter isLava allNeighbors)
    toVisit = S.union visited (S.fromList notVisitedNeighbors)
    newQueue = S.union (S.deleteAt 0 queue) (S.fromList (filter (not . isLava) notVisitedNeighbors))

main = do
  content <- readFile "input.txt"
  let lava = map parseCube . lines $ content

  let totalOpenSurface = sum $ map (\cube -> 6 - length (filter (`elem` lava) $ neighbors cube)) lava
  print $ "Solution 1 is " ++ show totalOpenSurface

  let (leftCorner, rightCorner) = getCorners lava

  let outerSurface = fill 0 (canBeVisited leftCorner rightCorner) (isLava lava) S.empty (S.singleton leftCorner)
  print $ "Solution 2 is " ++ show outerSurface
