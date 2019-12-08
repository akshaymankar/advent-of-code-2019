module Day5 where

import IntCode.Execute
import IntCode.Read
import IntCode.Types

day5_1 :: IO ()
day5_1 = do
  memory <- readMemory
  print
    $ diagnosticCode
    $ snd
    $ executeWith [1]
    $ mkExecution memory

day5_2 :: IO ()
day5_2 = do
  input <- readMemory
  print
    $ diagnosticCode
    $ snd
    $ executeWith [5]
    $ mkExecution input

diagnosticCode :: [Output] -> Int
diagnosticCode [] = error $ "No outputs found!"
diagnosticCode [x] = x
diagnosticCode (0:xs) = diagnosticCode xs
diagnosticCode (n:_) = error $ "Unexpected output: " ++ show n
