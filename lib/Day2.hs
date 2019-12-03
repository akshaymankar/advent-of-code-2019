module Day2 where

import Data.Char
import Data.Sequence
import Text.ParserCombinators.ReadP

day2_1 :: IO ()
day2_1 = do
  intcode <- readInput
  print $ executeIntcode $ withNounAndVerb 12 02 intcode

day2_2 :: IO ()
day2_2 = do
  intcode <- readInput
  let result = head [100 * noun + verb | noun <- [0..99]
                                       , verb <- [0..99]
                                       , executeIntcode (withNounAndVerb noun verb intcode) == 19690720
                                       ]
  print result

type Code = Seq Int
type Pos = Int
type Noun = Int
type Verb = Int

withNounAndVerb :: Noun -> Verb -> Code -> Code
withNounAndVerb noun verb code =
  update 1 noun $ update 2 verb code

executeIntcode :: Code -> Int
executeIntcode input =
  snd (go (0, input)) `index` 0
  where
    go :: (Pos, Code) -> (Pos, Code)
    go (pos, code) =
      let op = code `index` pos
      in case op of
        1 -> go (pos + 4, add (binaryOperands pos code) code)
        2 -> go (pos + 4, multiply (binaryOperands pos code) code)
        99 -> (pos, code)
        invalid -> error $ "Invalid operation: " ++ show invalid
    binaryOperands :: Pos -> Code -> (Int, Int, Int)
    binaryOperands pos code = ( code `index` (pos + 1)
                              , code `index` (pos + 2)
                              , code `index` (pos + 3)
                              )

add :: (Int, Int, Int) -> Code -> Code
add = binaryOperation (+)

multiply :: (Int, Int, Int) -> Code -> Code
multiply = binaryOperation (*)

binaryOperation :: (Int -> Int -> Int) -> (Int, Int, Int) -> Code -> Code
binaryOperation f (op1, op2, result) code =
   let a = code `index` op1
       b = code `index` op2
  in update result (a `f` b) code

readInput :: IO Code
readInput = do
  inputStr <- getLine
  case readP_to_S inputReadP inputStr of
    [(ints, "")] -> pure $ fromList ints
    _ -> error $ "Failed to parse '" ++ inputStr ++ "'"

inputReadP :: ReadP [Int]
inputReadP = do
  sepBy1 intReadP commaReadP <* eof

intReadP :: ReadP Int
intReadP = do
  intStr <- many1 $ satisfy isDigit
  pure $ read intStr

commaReadP :: ReadP ()
commaReadP = satisfy (== ',') *> pure ()
