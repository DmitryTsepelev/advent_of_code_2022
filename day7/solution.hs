{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List (isPrefixOf, find, sort)

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> drop 1 w : wordsWhen p s''
                            where (w, s'') = break p s'

data Node = Node { name :: String, children :: [Node], size :: Int } deriving (Show, Eq)

setChildren :: [String] -> [Node] -> Node -> Node
setChildren path childrenToSet node@(Node name children size)
  | currentDir /= name = node
  | currentDir == name && null restPath = Node name childrenToSet size
  | otherwise = Node name newChidlren size
  where
    (currentDir:restPath) = path
    newChidlren = map (setChildren restPath childrenToSet) children

-- Input parsing

processCommand :: (Node, [String]) -> [String] -> (Node, [String])
processCommand acc (cmd:output)
  | "cd" `isPrefixOf` cmd = processCd acc (last . words $ cmd)
  | "ls" `isPrefixOf` cmd = processLs acc output
  | otherwise = acc

processCd :: (Node, [String]) -> String -> (Node, [String])
processCd (root, currentPath) targetDir
  | targetDir == ".." = (root, init currentPath)
  | otherwise = (root, currentPath ++ [targetDir])

processLs :: (Node, [String]) -> [String] -> (Node, [String])
processLs (node, currentPath) output = (setChildren currentPath newChildren node, currentPath)
  where
    newChildren = map (buildNode . words) output

    buildNode [dirOrSize, name]
      | dirOrSize == "dir" = Node name [] 0
      | otherwise = Node name [] (read dirOrSize :: Int)

-- Calculations

calculateTotalSizes :: Node -> Node
calculateTotalSizes node@(Node name children _)
  | null children = node
  | otherwise = Node name updatedChildren (sum . map size $ updatedChildren)
  where
    updatedChildren = map calculateTotalSizes children

filterNodesBySize :: (Int -> Bool) ->[Int] ->  Node -> [Int]
filterNodesBySize predicate acc (Node _ children size)
  | null children || predicate size = filteredChildren acc
  | otherwise = filteredChildren (size:acc)
  where
    filteredChildren acc = foldl (filterNodesBySize predicate) acc children

solve1 :: Node -> Int
solve1 = sum . filterNodesBySize (> 100000) []

solve2 :: Node -> Int
solve2 root =
  head . filter (> requiredSpace - (totalAvaiableSpace - usedSpace)) . sort . filterNodesBySize (const False) [] $ root
  where
    totalAvaiableSpace = 70000000
    requiredSpace = 30000000
    usedSpace = size root

main = do
  content <- readFile "input.txt"

  let input = map lines . filter (not . null) . wordsWhen (== '$') $ content

  let root = calculateTotalSizes . fst . foldl processCommand (Node "/" [] 0, [])  $ input

  print $ "Solution 1 is " ++ show (solve1 root)
  print $ "Solution 2 is " ++ show (solve2 root)
