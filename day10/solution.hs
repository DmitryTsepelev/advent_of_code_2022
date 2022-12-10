{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

data Instruction = Noop | AddX Int deriving Show
type Program = [Instruction]

buildCommand :: [[Char]] -> Instruction
buildCommand ["noop"] = Noop
buildCommand ["addx", value] = AddX (read value :: Int)

type Cycles = Int
type X = Int
type VM = (Cycles, (X, X))

executeProgram :: Program -> [VM]
executeProgram = executeProgram' (0, (1, 1))

executeProgram' :: VM -> Program -> [VM]
executeProgram' _ [] = []
executeProgram' vm (instruction:rest) =
  instructionResult ++ executeProgram' (last instructionResult) rest
  where
    instructionResult = executeInstruction vm instruction

executeInstruction :: VM -> Instruction -> [VM]
executeInstruction (cycles, (_, afterX)) Noop = [(succ cycles, (afterX, afterX))]
executeInstruction (cycles, (_, afterX)) (AddX value) = [(cycles + 1, (afterX, afterX)), (cycles + 2, (afterX, afterX + value))]

type Trace = [VM]

solve1 :: Trace -> Int
solve1 = sum . map signalStrength . filterCycles
  where
    signalStrength (cycle, (startX, _)) = cycle * startX
    filterCycles = filter (\(cycle, _) -> cycle `elem` [20, 60, 100, 140, 180, 220])

replace :: Int -> Char -> String -> String
replace x value screen =
  start ++ [value] ++ end
  where (start, end) = splitAt x screen

calculateScreen :: Trace -> String -> String
calculateScreen [] screen = screen
calculateScreen ((cycle, (startX, afterX)):rest) screen =
  calculateScreen rest newScreen
  where
    newScreen
      | horizontalPosition `elem` [startX-1 .. startX+1] = replace (pred cycle) '#' screen
      | otherwise = screen
    horizontalPosition = (cycle `mod` 40) - 1

drawScreen :: String -> IO ()
drawScreen screen =
  mapM_ (\shift -> putStrLn . take 40 . drop shift $ screen) shifts
  where shifts = take 6 [0, 40 ..]

main = do
  content <- readFile "input.txt"
  let program = map (buildCommand . words) . lines $ content
  let trace = executeProgram program

  putStrLn $ "Solution 1 is " ++ show (solve1 trace)

  putStrLn "Solution 2 is "
  let screen = calculateScreen trace (replicate 240 '.')
  drawScreen screen
