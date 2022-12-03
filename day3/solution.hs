import Data.List (intersect)
import Data.Char (ord, isUpper)

findWrongItem :: String -> Char
findWrongItem rucksack = head $ compartment1 `intersect` compartment2
  where
    (compartment1, compartment2) = splitAt compartmentSize rucksack
    compartmentSize = length rucksack `div` 2

priorityOf :: Char -> Int
priorityOf item
  | isUpper item = 27 + ord item - ord 'A'
  | otherwise = 1 + ord item - ord 'a'

solve1 :: [String] -> Int
solve1 = sum . map (priorityOf . findWrongItem)

splitToGroups :: Int -> [a] -> [[a]]
splitToGroups = splitToGroups' []

splitToGroups' :: [[a]] -> Int -> [a] -> [[a]]
splitToGroups' acc groupSize [] = acc
splitToGroups' acc groupSize list =
  splitToGroups' (group:acc) groupSize rest
  where (group, rest) = splitAt groupSize list

findCommonItem :: [[Char]] -> Char
findCommonItem = head . foldl1 intersect

solve2 :: [[Char]] -> Int
solve2 = sum . map (priorityOf . findCommonItem) . splitToGroups 3

main = do
  content <- readFile "input.txt"
  let rucksacks = lines content

  print $ "Solution 1 is " ++ show (solve1 rucksacks)
  print $ "Solution 2 is " ++ show (solve2 rucksacks)
