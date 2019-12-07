module Day7 where

import Day5
import Data.List (permutations)

day7_1 :: IO ()
day7_1 = do
  input <- readInput
  print $ maximum $ map (calculateThrusterSignal input) (permutations [0..4])

type Setting = Int

calculateThrusterSignal :: Code -> [Setting] -> Int
calculateThrusterSignal amplifierController settings =
  foldr executeAmp 0 $ reverse settings
  where
    executeAmp setting input =
      case readOutputs $ execute [setting, input] amplifierController of
        [o] -> o
        os -> error $ "expected only one output, got " ++ show os
