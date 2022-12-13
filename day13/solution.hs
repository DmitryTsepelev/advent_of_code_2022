{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
import Control.Applicative ( Alternative(many, empty, (<|>)) )
import Data.Char (toLower, isDigit, digitToInt)
import Data.Maybe (fromJust)
import Data.Either (isRight)
import Data.List (isPrefixOf, findIndex, sort, elemIndex)
import Debug.Trace (trace)

-- Applicative parser from scratch

data Parser a = Parser { parse :: String -> Either String (String, a) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy pr = Parser f where
  f "" = Left "unexpected end of input"
  f (c:cs) = if pr c then Right (cs, c) else Left ("unexpected " ++ [c])

char :: Char -> Parser Char
char p = satisfy (== p)

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (fmap f) . p

instance Applicative Parser where
  pure a = Parser $ \s -> Right (s, a)

  pf <*> pv = Parser $ \s ->
    case parse pf s of
      Right (s', g) ->
        case parse pv s' of
          Right (s'', a) -> Right (s'', g a)
          Left e -> Left e
      Left e -> Left e

sepBy :: (Show a) => Parser a -> Parser sep -> Parser [a]
sepBy p sep = (\x xs -> x ++ [xs]) <$> many (p <* sep) <*> p

instance Alternative Parser where
  empty = Parser $ \s -> Left $ "unexpected " ++ s

  p <|> q = Parser f where
      f s = let ps = parse p s
        in if isRight ps then ps else parse q s

digit :: Parser Char
digit = satisfy isDigit

optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p =
  Parser $ \s ->
    let ps = parse p s
      in case ps of
        Right (s', a) -> if s' /= s then Right (s', Just a) else Right (s', Nothing)
        Left _ -> Right (s, Nothing)

listContents :: Parser [Packet Int]
listContents =
  char '[' *> (
    ((Leaf . read <$> many1 digit) <|> listP) `sepBy` char ','
  ) <* char ']'

listP :: Parser (Packet Int)
listP = Node <$> (listContents <|> pure [])

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

runParser :: Parser a -> String -> a
runParser p s | Right ("", a) <- parse p s = a
              | otherwise = error $ "failed to run parser" <> s

-- Input parsing

parsePairs :: [String] -> [(Packet Int, Packet Int)]
parsePairs [] = []
parsePairs (fst:snd:rest) =
  (runParser listP fst, runParser listP snd) : parsePairs (drop 1 rest)

-- Solution

data Packet a = Leaf a | Node [Packet a] deriving (Eq, Show)

instance (Ord a) => Ord (Packet a) where
  compare :: Ord a => Packet a -> Packet a -> Ordering
  compare (Leaf l1) (Leaf l2)
    | l1 < l2 = GT
    | l2 == l1 = EQ
    | otherwise = LT

  compare (Node n1) (Leaf l2) = compare (Node n1) (Node [Leaf l2])

  compare (Leaf l1) (Node n2) = compare (Node [Leaf l1]) (Node n2)

  compare (Node (v1:rest1)) (Node (v2:rest2)) =
    case compare v1 v2 of
      EQ -> compare (Node rest1) (Node rest2)
      result -> result

  compare (Node l) (Node r)
    | null r && not (null l) = LT
    | null l && not (null r) = GT
    | null l && null r = EQ

solve1 :: [(Packet Int, Packet Int)] -> Integer
solve1 = sum . map fst . filter (\(idx, (l, r)) -> l > r) . zip [1..]

solve2 :: [(Packet Int, Packet Int)] -> Int
solve2 pairs = product . map ((succ . fromJust) . (`elemIndex` sortedPackets)) $ dividerPackets
  where
    dividerPackets = [Node [Leaf 2], Node [Leaf 6]]
    allPackets = foldl (\acc (p1, p2) -> p1:p2:acc) [] pairs
    sortedPackets = reverse . sort $ dividerPackets ++ allPackets

main = do
  content <- readFile "input.txt"
  let pairs = parsePairs . lines $ content

  print $ "Solution 1 is " ++ show (solve1 pairs)
  print $ "Solution 2 is " ++ show (solve2 pairs)
