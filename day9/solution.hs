{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Concurrent ( threadDelay )
import Data.List (elemIndex)
import Data.Char (digitToInt, ord, chr)
import Data.Set (toList, fromList)
import Data.Maybe (fromJust)

buildCommands :: [([Char], Int)] -> [Position -> Position]
buildCommands = foldl (\acc (direction, steps) -> acc ++ replicate steps (commandFor direction)) []

type Position = (Int, Int)
type Rope = [Position]
type Leg = (Position, Position)

moveTail :: Position -> Position -> Position
moveTail (headX, headY) (tailX, tailY)
  | abs (headX - tailX) >= 2 = moveTailHorizontally (headX, headY) (tailX, tailY)
  | abs (headY - tailY) >= 2 = moveTailVertically (headX, headY) (tailX, tailY)
  | otherwise = (tailX, tailY)

moveTailHorizontally :: Position -> Position -> Position
moveTailHorizontally (headX, headY) (tailX, tailY)
  | headY == tailY = (tailX + delta, tailY)
  | headY > tailY = (tailX + delta, tailY + 1)
  | otherwise = (tailX + delta, tailY - 1)
  where delta = if headX > tailX then headX - tailX - 1 else headX - tailX + 1

moveTailVertically :: Position -> Position -> Position
moveTailVertically (headX, headY) (tailX, tailY)
  | headX == tailX = (tailX, tailY + delta)
  | headX > tailX = (tailX + 1, tailY + delta)
  | otherwise = (tailX - 1, tailY + delta)
  where delta = if headY > tailY then headY - tailY - 1 else headY - tailY + 1

moveHorizontally :: Int -> Position -> Position
moveHorizontally delta (headX, headY) = (headX + delta, headY)

moveVertically :: Int -> Position -> Position
moveVertically delta (headX, headY) = (headX, headY + delta)

commandFor :: [Char] -> Position -> Position
commandFor "R" = moveHorizontally 1
commandFor "L" = moveHorizontally (-1)
commandFor "U" = moveVertically 1
commandFor "D" = moveVertically (-1)

buildPath :: Int -> [Position -> Position] -> [Rope]
buildPath ropeLength =
  foldl (\path fn -> path ++ [moveRope fn (last path)]) [startRope]
  where startRope = replicate ropeLength (0, 0)

moveRope :: (Position -> Position) -> Rope -> Rope
moveRope fn (knot:rest) = newKnot : moveTails newKnot rest
  where
    newKnot = fn knot

    moveTails head [] = []
    moveTails head (tail:rest) =
      newTail:moveTails newTail rest
      where newTail = moveTail head tail

uniqueTailPositions ::  Int -> [Position -> Position] -> Int
uniqueTailPositions ropeLength commands = length . toList . fromList . map last $ buildPath ropeLength commands

-- Visualize

clear :: IO ()
clear = putStr "\ESC[H"

drawPath :: Int -> [Rope] -> IO ()
drawPath size [] = return ()
drawPath size (rope:rest) = do
  clear
  drawField size rope
  threadDelay 50000
  drawPath size rest

drawField :: Int -> Rope -> IO ()
drawField size rope =
  mapM_ (\y -> putStrLn $ map (`symbol` y) coordinates) (reverse coordinates)
  where
    shift = size `div` 2
    coordinates = [-shift..shift]
    idx x y = fromJust (elemIndex (x, y) rope)
    symbol x y
      | (x, y) `elem` rope && (idx x y == 0) = 'H'
      | (x, y) `elem` rope && (idx x y == (length rope - 1)) = 'T'
      | (x, y) `elem` rope = '*'
      | y == 0 = '—'
      | x == 0 = '|'
      | otherwise = '.'

main = do
  content <- readFile "input.txt"
  let input = map ((\cmp -> (head cmp, read (cmp!!1) :: Int)) . words) . lines $ content
  let commands = buildCommands input

  -- uncomment to visualize
  -- clear
  -- drawPath 40 $ buildPath 10 commands

  print $ "Solution 1 is " ++ show (uniqueTailPositions 2 commands)
  print $ "Solution 2 is " ++ show (uniqueTailPositions 10 commands)
