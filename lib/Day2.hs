module Day2 where

import Text.ParserCombinators.ReadP
import Data.Char
import Data.Array

day2_1 :: IO ()
day2_1 = do
  intcode <- readInput
  print $ executeIntcode $ withNounAndVerb 12 02 intcode

day2_2 :: IO ()
day2_2 = do
  intcode <- readInput
  let result = head [100 * noun + verb | noun <- [0..99]
                                       , verb <- [0..99]
                                       , let res = executeIntcode (withNounAndVerb noun verb intcode)
                                         in res == 19690720]
  print result

type Code = Array Int Int
type Pos = Int
type Noun = Int
type Verb = Int

withNounAndVerb :: Noun -> Verb -> Code -> Code
withNounAndVerb noun verb code = code // [(1,noun), (2,verb)]

executeIntcode :: Code -> Int
executeIntcode input =
  snd (go (0, input)) ! 0
  where
    go :: (Pos, Code) -> (Pos, Code)
    go (pos, code) =
      let op = code ! pos
      in case op of
        1 -> go (pos + 4, add (binaryOperands pos code) code)
        2 -> go (pos + 4, multiply (binaryOperands pos code) code)
        99 -> (pos, code)
        invalid -> error $ "Invalid operation: " ++ show invalid
    binaryOperands :: Pos -> Code -> (Int, Int, Int)
    binaryOperands pos code = (code ! (pos + 1), code ! (pos + 2), code ! (pos + 3))

add :: (Int, Int, Int) -> Code -> Code
add = binaryOperation (+)

multiply :: (Int, Int, Int) -> Code -> Code
multiply = binaryOperation (*)

binaryOperation :: (Int -> Int -> Int) -> (Int, Int, Int) -> Code -> Code
binaryOperation f (op1, op2, result) code =
   let a = code ! op1
       b = code ! op2
  in code // [(result, a `f` b)]

readInput :: IO Code
readInput = do
  inputStr <- getLine
  case readP_to_S inputReadP inputStr of
    [(ints, "")] -> pure $ listArray (0, length ints - 1) ints
    _ -> error $ "Failed to parse '" ++ inputStr ++ "'"

inputReadP :: ReadP [Int]
inputReadP = do
  allButLast <- many1 $ intReadP <* commaReadP
  lastOne <- intReadP
  _ <- eof
  pure $ allButLast ++ [lastOne]

intReadP :: ReadP Int
intReadP = do
  intStr <- many1 $ satisfy isDigit
  pure $ read intStr

commaReadP :: ReadP ()
commaReadP = satisfy (== ',') *> pure ()
