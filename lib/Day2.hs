module Day2 where

import Data.Sequence
import Day5

day2_1 :: IO ()
day2_1 = do
  intcode <- readInput
  print $ readPos0 $ execute 0 $ withNounAndVerb 12 02 intcode

day2_2 :: IO ()
day2_2 = do
  intcode <- readInput
  let result = head [100 * noun + verb | noun <- [0..99]
                                       , verb <- [0..99]
                                       , readPos0 (execute 0 (withNounAndVerb noun verb intcode)) == 19690720
                                       ]
  print result

type Noun = Int
type Verb = Int

withNounAndVerb :: Noun -> Verb -> Code -> Code
withNounAndVerb noun verb =
  (update 1 noun) . (update 2 verb)

readPos0 :: ExecutionState -> Int
readPos0 s = code s `index` 0
