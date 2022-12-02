{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Bifunctor (second)
import Data.Tuple (swap)
import Data.List (find)
import Data.Maybe (fromJust)

winCombinations :: [(Shape, Shape)]
winCombinations = [(Rock, Paper), (Paper, Scissors), (Scissors, Rock)]

looseCombinations :: [(Shape, Shape)]
looseCombinations = map swap winCombinations

data Shape = Rock | Scissors | Paper deriving (Eq, Show)
instance Ord Shape where
  compare shape1 shape2
    | (shape1, shape2) `elem` winCombinations = GT
    | (shape1, shape2) `elem` looseCombinations = LT
    | otherwise = EQ

data RoundResult = NeedWin | NeedDraw | NeedLoose

type ShapeAndResultInstruction = (Shape, RoundResult)
type Instructions = [(Shape, RoundResult)]

parseInstruction :: [String] -> [ShapeAndResultInstruction]
parseInstruction =
  map (\row -> (parseShape $ head row, parseExpectedResult $ row!!2))
  where
    parseShape 'A' = Rock
    parseShape 'B' = Paper
    parseShape 'C' = Scissors

    parseExpectedResult 'X' = NeedLoose
    parseExpectedResult 'Y' = NeedDraw
    parseExpectedResult 'Z' = NeedWin

roundResultToShape :: RoundResult -> Shape
roundResultToShape NeedLoose = Rock
roundResultToShape NeedDraw = Paper
roundResultToShape NeedWin = Scissors

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

roundResultScore :: (Shape, Shape) -> Int
roundResultScore (shape1, shape2)
  | shape1 > shape2 = 6
  | shape1 < shape2 = 0
  | otherwise = 3

roundScore :: (Shape, Shape) -> Int
roundScore instruction = shapeScore (snd instruction) + roundResultScore instruction

chooseShape :: ShapeAndResultInstruction -> Shape
chooseShape (shape, result) =
  case result of
    NeedDraw -> shape
    NeedWin -> findExpectedShape shape winCombinations
    NeedLoose -> findExpectedShape shape looseCombinations
  where
    findExpectedShape shape combinations =
      snd . fromJust . find (\(f, s) -> shape == f) $ combinations

solve1 :: Instructions -> Int
solve1 = sum . map (roundScore . second roundResultToShape)

solve2 :: Instructions -> Int
solve2 = sum . map (roundScore . (\(f, s) -> (f, chooseShape (f, s))))

main = do
  content <- readFile "input.txt"
  let instruction = parseInstruction . lines $ content

  print $ "Solution 1 is " ++ show (solve1 instruction)
  print $ "Solution 2 is " ++ show (solve2 instruction)
