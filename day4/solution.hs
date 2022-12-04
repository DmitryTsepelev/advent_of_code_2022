{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- https://stackoverflow.com/a/7569301
splitBy :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitBy delimiter = foldr f [[]]
            where f c l@(x:xs) | c == delimiter = []:l
                             | otherwise = (c:x):xs

type Range = (Int, Int)

parseAssignments :: [String] -> [(Range, Range)]
parseAssignments = map parseAssignmentPair

parseAssignmentPair :: String -> (Range, Range)
parseAssignmentPair string = (fst, snd)
  where [fst, snd] = map parseAssignment . splitBy ',' $ string

parseAssignment :: String -> Range
parseAssignment s = (read fst :: Int, read snd :: Int) where [fst, snd] = splitBy '-' s

fullyContains :: (Range, Range) -> Bool
fullyContains ((l1, r1), (l2, r2)) = l1 >= l2 && r1 <= r2 || l2 >= l1 && r2 <= r1

solve1 :: [(Range, Range)] -> Int
solve1 = length . filter fullyContains

overlaps :: (Range, Range) -> Bool
overlaps ((l1, r1), (l2, r2)) = minimum [r1, r2] - maximum [l1, l2] >= 0

solve2 :: [(Range, Range)] -> Int
solve2 = length . filter overlaps

main = do
  content <- readFile "input.txt"
  let assginments = parseAssignments . lines $ content

  print $ "Solution 1 is " ++ show (solve1 assginments)
  print $ "Solution 2 is " ++ show (solve2 assginments)
