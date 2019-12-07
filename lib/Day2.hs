module Day2 where

import Data.Sequence
import Day5

day2_1 :: IO ()
day2_1 = do
  intcode <- readMemory
  print $ executeAndReadPos0 $ withNounAndVerb 12 02 intcode

day2_2 :: IO ()
day2_2 = do
  intcode <- readMemory
  let result = head [100 * noun + verb | noun <- [0..99]
                                       , verb <- [0..99]
                                       , executeAndReadPos0 (withNounAndVerb noun verb intcode) == 19690720
                                       ]
  print result

type Noun = Int
type Verb = Int

withNounAndVerb :: Noun -> Verb -> Code -> Code
withNounAndVerb noun verb =
  (update 1 noun) . (update 2 verb)

readPos0 :: Memory -> Int
readPos0 s = code s `index` 0

executeAndReadPos0 :: Code -> Int
executeAndReadPos0 = readPos0 . fst . executeWith [] . mkExecution
