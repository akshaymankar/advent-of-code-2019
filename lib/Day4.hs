module Day4 where

import Text.ParserCombinators.ReadP
import Data.Char

day4_1 :: IO ()
day4_1 = do
  (lower, upper) <- readInput
  print $ countPossibilities lower upper

countPossibilities :: Int -> Int -> Int
countPossibilities lower upper =
  let rules = combineRules [isSixDigits, digitsOnlyGrow, hasTwoAdjacentDigits]
  in length $ filter rules [lower..upper]

combineRules :: [(a -> Bool)] -> a -> Bool
combineRules rules x = foldr (\f acc -> acc &&  f x) True rules

isSixDigits :: Int -> Bool
isSixDigits n = n > 99999 && n < 1000000

digitsOnlyGrow :: Int -> Bool
digitsOnlyGrow n =
  go $ toDigits n
  where
    go :: [Int] -> Bool
    go [] = True
    go [_] = True
    go (d1:d2:ds) = d1 >= d2 && go (d2:ds)

hasTwoAdjacentDigits :: Int -> Bool
hasTwoAdjacentDigits n =
  go $ toDigits n
  where
    go :: [Int] -> Bool
    go [] = False
    go [_] = False
    go (d1:d2:ds)
      | d1 == d2 = True
      | otherwise = go (d2:ds)

toDigits :: Int -> [Int]
toDigits n
  | n == 0 = []
  | otherwise = n `mod` 10 : toDigits (n `div` 10)

readInput :: IO (Int, Int)
readInput = do
  inputStr <- getLine
  case readP_to_S inputReadP inputStr of
    [(range, "")] -> pure range
    _ -> error $ "Failed to parse '" ++ inputStr ++ "'"

inputReadP :: ReadP (Int, Int)
inputReadP = do
  lower <- intReadP
  _ <- char '-'
  upper <- intReadP
  _ <- eof
  pure (lower, upper)

intReadP :: ReadP Int
intReadP = do
  intStr <- many1 $ satisfy isDigit
  pure $ read intStr

