module Day2 where

import IntCode.Types
import IntCode.Read
import IntCode.Execute

day2_1 :: IO ()
day2_1 = do
  intcode <- parseMemory
  print $ executeAndReadPos0 $ withNounAndVerb 12 02 intcode

day2_2 :: IO ()
day2_2 = do
  intcode <- parseMemory
  let result = head [100 * noun + verb | noun <- [0..99]
                                       , verb <- [0..99]
                                       , executeAndReadPos0 (withNounAndVerb noun verb intcode) == 19690720
                                       ]
  print result

type Noun = Int
type Verb = Int

withNounAndVerb :: Noun -> Verb -> Code -> Code
withNounAndVerb noun verb =
  (persistAt 1 noun) . (persistAt 2 verb)

readPos0 :: Memory -> Int
readPos0 = readPos 0 . code

executeAndReadPos0 :: Code -> Int
executeAndReadPos0 = readPos0 . fst . executeWith [] . mkExecution
