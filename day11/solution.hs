{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List (sort, isPrefixOf)
import Data.Char (digitToInt)
data Monkey = Monkey {
  monkeyId :: Int,
  monkeyItems :: [Int],
  monkeyDivisor :: Int,
  monkeyOperation :: Int -> Int,
  chooseMonkeyToThrow :: Int -> Int,
  inspections :: Int
}

instance Show Monkey where
  show (Monkey id items _ _ _ inspections) = "Monkey " ++ show id ++ ", items: " ++ show items ++ ", inspections: " ++ show inspections

parseMonkeys [] = []
parseMonkeys lines =
  parseMonkey (take 6 lines) : parseMonkeys (drop 7 lines)

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

parseMonkey lines =
  Monkey id items divisor operation chooseMonkeyToThrow 0
  where
    id = digitToInt . (!! 7) $ head lines
    items = map (\word -> read word :: Int) . wordsWhen (== ',') . drop 18 $ lines!!1

    divisor = read (drop 21 $ lines!!3) :: Int

    operation
      | "  Operation: new = old * old" `isPrefixOf` (lines!!2) = \x -> x * x
      | "  Operation: new = old * " `isPrefixOf` (lines!!2) =
        (* (read (drop 25 $ lines!!2) :: Int))
      | "  Operation: new = old + " `isPrefixOf` (lines!!2) =
        (+ (read (drop 25 $ lines!!2) :: Int))

    chooseMonkeyToThrow level =
      if level `mod` divisor == 0 then
        digitToInt (lines!!4!!29)
      else
        digitToInt (lines!!5!!30)

runRound :: (Int -> Int) -> [Monkey] -> [Monkey]
runRound reduceWorry monkeys =
  foldl (monkeyTurn reduceWorry) monkeys [0..length monkeys - 1]

replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx value list =
  start ++ [value] ++ end
  where (start,_:end) = splitAt idx list

monkeyTurn :: (Int -> Int) -> [Monkey] -> Int -> [Monkey]
monkeyTurn reduceWorry monkeys currentId = do
  let updadedMonkeys = replaceAt id (Monkey id [] divisor operation monkeyToThrow (inspections + length items)) monkeys
  foldl (throwItem reduceWorry operation monkeyToThrow) updadedMonkeys items

  where
    (Monkey id items divisor operation monkeyToThrow inspections) = monkeys!!currentId

throwItem :: (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> [Monkey] -> Int -> [Monkey]
throwItem reduceWorry operation monkeyToThrow monkeys item = do
  let worryLevel = reduceWorry $ operation item
  let monkeyIdToSend = monkeyToThrow worryLevel
  replaceAt monkeyIdToSend (addItem (monkeys!!monkeyIdToSend) worryLevel) monkeys

addItem :: Monkey -> Int -> Monkey
addItem (Monkey id items divisor operation monkeyToThrow inspections) item =
  Monkey id (items ++ [item]) divisor operation monkeyToThrow inspections

runSimulation 0 _ monkeys = monkeys
runSimulation rounds reduceWorry monkeys = runSimulation (pred rounds) reduceWorry (runRound reduceWorry monkeys)

solve1 reduceWorry roundCount = product . take 2 . reverse . sort . map inspections . runSimulation roundCount reduceWorry

main = do
  content <- readFile "input.txt"
  let input = lines content
  let monkeys = parseMonkeys input

  print $ solve1 (`div` 3) 20 monkeys
  print $ solve1 (`mod` (product $ map monkeyDivisor monkeys)) 10000 monkeys
