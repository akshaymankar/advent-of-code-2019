module IntCode.Types where

import Data.IntMap

type Input = Int
type Output = Int
type Code = IntMap Int
type Pos = Int

data Memory = Memory { pos :: Pos
                     , code :: Code
                     , relativeBase :: Pos
                     }
              deriving Show

data Mode = Position
          | Immediate
          | Relative
          deriving Show

data Instruction = Add Mode Mode Mode
                 | Multiply Mode Mode Mode
                 | Input Mode
                 | Output Mode
                 | JumpIfTrue Mode Mode
                 | JumpIfFalse Mode Mode
                 | LessThan Mode Mode Mode
                 | Equals Mode Mode Mode
                 | AdjustRelativeBase Mode
                 | Halt
              deriving Show

data Signal = SignalOutput Output
            | SignalHalted Memory
            deriving Show
