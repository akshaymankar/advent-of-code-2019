module Day2 where

import Text.ParserCombinators.ReadP
import Data.Char
import Data.Array

day2_1 :: IO ()
day2_1 = do
  ints <- readInput
  let intcode = listArray (0, length ints - 1) ints
      result = executeIntcode $ replace1202 intcode
  print $ result ! 0

type Code = Array Int Int
type Pos = Int

replace1202 :: Code -> Code
replace1202 code = code // [(1,12), (2,2)]

executeIntcode :: Code -> Code
executeIntcode input =
  snd $ go (0, input)
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

readInput :: IO [Int]
readInput = do
  inputStr <- getLine
  case readP_to_S inputReadP inputStr of
    [(inputs, "")] -> pure inputs
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
