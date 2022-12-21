{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import qualified Data.Map as M

data Instruction = Const Int | Function (Monkeys -> Int)
type Monkeys = M.Map String Instruction

parseLine :: String -> (String, Instruction)
parseLine s = (name, fn)
  where
    name = take 4 s
    fn = buildFn (drop 5 s)

buildFn :: String -> Instruction
buildFn s
  | length cmp == 1 = Function $ const (read (head cmp) :: Int)
  | otherwise = Function $ \monkeys -> fn (resolve monkeys (head cmp)) (resolve monkeys (last cmp))
  where
    cmp = words s
    fn = case cmp!!1 of
      "+" -> (+)
      "-" -> (-)
      "*" -> (*)
      "/" -> div

resolve :: Monkeys -> String -> Int
resolve monkeys name = do
  let fn = monkeys M.! name
  case fn of
    (Const value) -> value
    (Function fn) -> fn monkeys



main = do
  content <- readFile "input.txt"
  let monkeys = M.fromList . map parseLine . lines $ content
  print $ resolve monkeys "root"
