import Data.List (sort)

-- from https://hackage.haskell.org/package/list-grouping-0.1.1/docs/src/Data-List-Grouping.html#breakDrop
breakDrop :: (a -> Bool) -> [a] -> [[a]]
breakDrop p = next . break p . dropWhile p
    where next ([], _) = []
          next (as,bs) = as : breakDrop p bs

main = do
  content <- readFile "input.txt"
  let rows = lines content

  let allWeights = fmap (\s -> read s :: Int) <$> breakDrop (== "") rows
  let sortedWeightGroups = reverse . sort . map sum $ allWeights

  let maxWeight = head sortedWeightGroups
  print $ "Solution 1 is " ++ show maxWeight

  let top3MaxWeights = sum . take 3 $ sortedWeightGroups

  print $ "Solution 2 is " ++ show top3MaxWeights
