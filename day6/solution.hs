import Data.Set (toList, fromList)

isUnique :: String -> Bool
isUnique list = length list == (length . toList . fromList $ list)

findUniqueSequence :: Int -> String -> Int
findUniqueSequence = findUniqueSequence' 0

findUniqueSequence' :: Int -> Int -> String -> Int
findUniqueSequence' idx length line
  | isUnique chars = idx + length
  | otherwise = findUniqueSequence' (succ idx) length (tail line)
  where chars = take length line

main = do
  content <- readFile "input.txt"
  let line = head . lines $ content

  print $ "Solution 1 is " ++ show (findUniqueSequence 4 line)
  print $ "Solution 2 is " ++ show (findUniqueSequence 14 line)
