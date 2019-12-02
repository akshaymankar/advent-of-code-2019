{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment
import System.Exit
import Day1
import Day2

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
    x -> die $ "'" ++ x ++ "' is not implemented yet."
