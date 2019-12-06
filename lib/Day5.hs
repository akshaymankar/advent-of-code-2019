{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Day5 where

import qualified Day2
import Data.Char
import Data.Sequence
import Text.ParserCombinators.ReadP

day5_1 :: IO ()
day5_1 = do
  input <- readInput
  print $ diagnosticCode $ outputs $ execute 1 input

day5_2 :: IO ()
day5_2 = do
  input <- readInput
  print $ diagnosticCode $ outputs $ execute 5 input

type Output = Int
data ExecutionState = ExecutionState { pos :: Day2.Pos
                                     , code :: Day2.Code
                                     , outputs :: [Output]
                                     }
                    deriving Show
data Mode = Position
          | Immediate
          deriving Show

data Instruction = Add Mode Mode
                 | Multiply Mode Mode
                 | Input
                 | Output Mode
                 | JumpIfTrue Mode Mode
                 | JumpIfFalse Mode Mode
                 | LessThan Mode Mode
                 | Equals Mode Mode
                 | Halt
                 deriving Show

diagnosticCode :: [Output] -> Int
diagnosticCode [] = error $ "No outputs found!"
diagnosticCode [x] = x
diagnosticCode (0:xs) = diagnosticCode xs
diagnosticCode (n:_) = error $ "Unexpected output: " ++ show n

execute :: Int -> Day2.Code -> ExecutionState
execute i c = do
  go (ExecutionState 0 c [])
  where
    go :: ExecutionState -> ExecutionState
    go state@ExecutionState{..} =
      let op = interpretOpCode (code `index` pos)
      in case op of
        Add m1 m2 ->
          executeBinOp add m1 m2
        Multiply m1 m2 ->
          executeBinOp multiply m1 m2
        LessThan m1 m2 ->
          executeBinOp lessThan m1 m2
        Equals m1 m2 ->
          executeBinOp equals m1 m2

        Input ->
          go state{ pos = pos + 2
                  , code = update (code `index` (pos + 1)) i code
                  }
        Output m ->
          go state{ pos = pos + 2
                  , outputs = outputs ++ [readOperand m (pos + 1)]
                  }

        JumpIfTrue m1 m2 ->
          executeJump (/= 0) m1 m2
        JumpIfFalse m1 m2 ->
          executeJump (== 0) m1 m2

        Halt -> state
      where
      executeBinOp :: BinaryOperation -> Mode -> Mode -> ExecutionState
      executeBinOp op m1 m2 =
        let operands = ( readOperand m1 (pos + 1)
                       , readOperand m2 (pos + 2)
                       , code `index` (pos + 3)
                       )
        in go (state{ pos = pos + 4
                    , code = op operands code
                    })

      executeJump :: (Int -> Bool) -> Mode -> Mode -> ExecutionState
      executeJump predicate m1 m2 =
        if predicate (readOperand m1 (pos + 1))
        then go (state{ pos = readOperand m2 (pos + 2) })
        else go (state{ pos = pos + 3 })

      readOperand :: Mode -> Day2.Pos -> Int
      readOperand Immediate p = code `index` p
      readOperand Position p = readOperand Immediate (code `index` p)

type BinaryOperation = ((Int, Int, Day2.Pos) -> Day2.Code -> Day2.Code)

add :: BinaryOperation
add = binaryOperation (+)

multiply :: BinaryOperation
multiply = binaryOperation (*)

lessThan :: BinaryOperation
lessThan = binaryOperation (\x y -> if x < y then 1 else 0)

equals :: BinaryOperation
equals = binaryOperation (\x y -> if x == y then 1 else 0)

binaryOperation :: (Int -> Int -> Int) -> BinaryOperation
binaryOperation f (op1, op2, result) code = update result (op1 `f` op2) code

interpretMode :: Int -> Mode
interpretMode =
  \case
    0 -> Position
    1 -> Immediate
    n -> error $ "Invalid mode: "  ++ show n

interpretOpCode :: Int -> Instruction
interpretOpCode n =
  case twoRightMostDigits of
    1 -> interpretBinaryOp Add
    2 -> interpretBinaryOp Multiply
    3 -> Input
    4 -> Output firstMode
    5 -> interpretBinaryOp JumpIfTrue
    6 -> interpretBinaryOp JumpIfFalse
    7 -> interpretBinaryOp LessThan
    8 -> interpretBinaryOp Equals
    99 -> Halt
    _ -> error $ "Invalid op code: " ++ show n
  where
    interpretBinaryOp constructor =
      constructor firstMode secondMode
    firstMode = interpretMode hundreds
    secondMode = interpretMode thousands
    twoRightMostDigits = n `mod` 100
    hundreds = somethings 100
    thousands = somethings 1000
    somethings thing = (n `div` thing) `mod` 10

readInput :: IO Day2.Code
readInput = do
  inputStr <- getLine
  case readP_to_S inputReadP inputStr of
    [(ints, "")] -> pure $ fromList ints
    lol -> error $ "Failed to parse '" ++ inputStr ++ "'" ++ show lol

inputReadP :: ReadP [Int]
inputReadP = do
  sepBy1 intReadP commaReadP <* eof

intReadP :: ReadP Int
intReadP = do
  signed <- choice [Just <$> char '-', pure Nothing]
  intStr <- many1 $ satisfy isDigit
  case signed of
    Nothing -> pure $ read intStr
    Just _ -> pure $ read ('-':intStr)

commaReadP :: ReadP ()
commaReadP = satisfy (== ',') *> pure ()
