module IntCode.Types where

import Data.Sequence

type Input = Int
type Output = Int
type Code = Seq Int
type Pos = Int

data Memory = Memory { pos :: Pos
                     , code :: Code
                     }

data Mode = Position
          | Immediate

data Instruction = Add Mode Mode
                 | Multiply Mode Mode
                 | Input
                 | Output Mode
                 | JumpIfTrue Mode Mode
                 | JumpIfFalse Mode Mode
                 | LessThan Mode Mode
                 | Equals Mode Mode
                 | Halt

data Signal = SignalOutput Output
            | SignalHalted Memory
