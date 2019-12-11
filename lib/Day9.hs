module Day9 where

import IntCode.Read
import IntCode.Execute
import IntCode.Types

day9_1 :: IO ()
day9_1 = do
  c <- parseMemory
  let (_, output) = executeWith [1] $ mkExecution c
  print output

day9_2 :: IO ()
day9_2 = do
  c <- parseMemory
  let (_, output) = executeWith [2] $ mkExecution c
  print output
