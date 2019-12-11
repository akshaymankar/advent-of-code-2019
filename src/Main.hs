{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment
import System.Exit
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10

main :: IO ()
main = do
  args <- getArgs
  case args of
    [name] -> run name
    _ ->
      die "Expected one param with in format day<num>-<num>"

run :: String -> IO ()
run =
  \case
    "day1-1" -> day1_1
    "day1-2" -> day1_2
    "day2-1" -> day2_1
    "day2-2" -> day2_2
    "day3-1" -> day3_1
    "day3-2" -> day3_2
    "day4-1" -> day4_1
    "day4-2" -> day4_2
    "day5-1" -> day5_1
    "day5-2" -> day5_2
    "day6-1" -> day6_1
    "day6-2" -> day6_2
    "day7-1" -> day7_1
    "day7-2" -> day7_2
    "day8-1" -> day8_1
    "day8-2" -> day8_2
    "day9-1" -> day9_1
    "day9-2" -> day9_2
    "day10-1" -> day10_1
    "day10-2" -> day10_2
    x -> die $ "'" ++ x ++ "' is not implemented yet."
