{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module IntCode.Execute where

import Data.Sequence
import Data.Functor.Identity
import IntCode.Types
import Pipes
import qualified Pipes.Prelude as P

type Execution = Pipe Input Signal Identity ()
type Execution' m = Pipe Input Signal m ()

separateMemoryAndOutputs :: [Signal] -> (Memory, [Output])
separateMemoryAndOutputs ss =
  let memories = foldr (\s ms -> case s of
                                   SignalHalted m -> m:ms
                                   _ -> ms
                       ) [] ss
      outputs = foldr (\s os -> case s of
                                  SignalOutput o -> o:os
                                  _ -> os
                      ) [] ss
  in (head memories, outputs)

executeWith :: [Input] -> Execution -> (Memory, [Output])
executeWith is r =
  let inputProducer :: Producer' Input Identity ()
      inputProducer = each is
      outputProducer :: Producer' Signal Identity ()
      outputProducer = inputProducer >-> r
  in consumeExecution outputProducer

consumeExecution :: Producer' Signal Identity () -> (Memory, [Output])
consumeExecution p = separateMemoryAndOutputs $ P.toList p

consumeExecutionIO :: Producer' Signal IO () -> IO (Memory, [Output])
consumeExecutionIO p = do
  signals <- P.toListM p
  return $ separateMemoryAndOutputs signals

mkExecution :: Functor m => Code -> Execution' m
mkExecution c =
  go (Memory 0 c)
  where
    go :: Functor m => Memory -> Execution' m
    go memory@Memory{..} =
      let op = interpretOpCode (code `index` pos)
      in case op of
        Add m1 m2 ->
          go $ executeBinOp add m1 m2
        Multiply m1 m2 ->
          go $ executeBinOp multiply m1 m2
        LessThan m1 m2 ->
          go $ executeBinOp lessThan m1 m2
        Equals m1 m2 ->
          go $ executeBinOp equals m1 m2

        Input -> do
          input <- await
          go memory{ pos = pos + 2
                   , code = update (code `index` (pos + 1)) input code
                   }
        Output m -> do
          yield $ SignalOutput $ readOperand m (pos + 1)
          go memory{ pos = pos + 2 }

        JumpIfTrue m1 m2 ->
          go $ executeJump (/= 0) m1 m2
        JumpIfFalse m1 m2 ->
          go $ executeJump (== 0) m1 m2

        Halt -> yield $ SignalHalted memory
      where
      executeBinOp :: BinaryOperation -> Mode -> Mode -> Memory
      executeBinOp op m1 m2 =
        let operands = ( readOperand m1 (pos + 1)
                       , readOperand m2 (pos + 2)
                       , code `index` (pos + 3)
                       )
        in memory{ pos = pos + 4
                 , code = op operands code
                 }

      executeJump :: (Int -> Bool) -> Mode -> Mode -> Memory
      executeJump predicate m1 m2 =
        if predicate (readOperand m1 (pos + 1))
        then memory{ pos = readOperand m2 (pos + 2) }
        else memory{ pos = pos + 3 }

      readOperand :: Mode -> Pos -> Int
      readOperand Immediate p = code `index` p
      readOperand Position p = readOperand Immediate (code `index` p)

type BinaryOperation = ((Int, Int, Pos) -> Code -> Code)

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
