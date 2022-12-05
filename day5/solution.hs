import Data.List (isPrefixOf, transpose)

type Crate = Char
type Stack = [Crate]
type Stacks = [Stack]

data Instruction = Instruction Int Int Int deriving Show

extractEvery :: Integral a => a -> [b] -> [b]
extractEvery m = map snd . filter (\(x,y) -> mod x m == 0) . zip [0..]

parseInput :: [String] -> (Stacks, [Instruction])
parseInput input =
  (stacks, instructions)
  where
    stacksAndIndexes = takeWhile (not . (`isPrefixOf` " 1")) input
    stacks = prepareInitialStacks stacksAndIndexes
    instructions = parseInstructions . drop ((succ . length) stacksAndIndexes) $ input

prepareInitialStacks :: [[Char]] -> [[Char]]
prepareInitialStacks = map (dropWhile (== ' ')) . transpose . map (extractEvery 4 . tail) . init

parseInstructions :: [String] -> [Instruction]
parseInstructions = map parseInstruction

parseInstruction :: String -> Instruction
parseInstruction line = Instruction crateCount from to
  where
    toInt value = read value :: Int
    crateCount = toInt . takeWhile (/= ' ') . drop 5 $ line
    from = toInt . take 1 . drop 5 . dropWhile (/= 'f') $ line
    to = toInt . drop ((pred . length) line) $ line

moveCrates :: Bool -> Stacks -> Instruction -> Stacks
moveCrates is9001 stacks (Instruction count from to) =
  zipWith (curry buildStack) [1..] stacks
  where
    movedCrates = orderCrates . take count . (!!) stacks . pred $ from
    orderCrates
      | is9001 = id
      | otherwise = reverse

    buildStack (idx, stack)
      | idx == from = drop count stack
      | idx == to = movedCrates ++ stack
      | otherwise = stack

simulate :: Bool -> Stacks -> [Instruction] -> [Crate]
simulate is9001 stacks = map head . foldl (moveCrates is9001) stacks

main = do
  content <- readFile "input.txt"
  let (stacks, instructions) = parseInput $ lines content

  print $ "Solution 1 is " ++ show (simulate False stacks instructions)
  print $ "Solution 2 is " ++ show (simulate True stacks instructions)
