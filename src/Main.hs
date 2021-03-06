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
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17

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
    "day11-1" -> day11_1
    "day11-2" -> day11_2
    "day12-1" -> day12_1
    "day12-2" -> day12_2
    "day13-1" -> day13_1
    "day13-2" -> day13_2
    "day14-1" -> day14_1
    "day14-2" -> day14_2
    "day15-1" -> day15_1
    "day15-2" -> day15_2
    "day16-1" -> day16_1
    "day16-2" -> day16_2
    "day17-1" -> day17_1
    "day17-2" -> day17_2
    x -> die $ "'" ++ x ++ "' is not implemented yet."
