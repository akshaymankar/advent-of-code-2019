{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment
import System.Exit
import Day1

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
    x -> die $ "'" ++ x ++ "' is not implemented yet."
