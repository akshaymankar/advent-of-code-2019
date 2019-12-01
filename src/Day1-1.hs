module Main where

import Control.Exception
import System.IO.Error

main :: IO ()
main = do
  total <- loop 0
  print total
    where
      loop total = do
        maybeMass <- catch
                     (Just <$> (readLn :: IO Int))
                     (\e -> if isEOFError e
                            then return Nothing
                            else throw e)
        case maybeMass of
          Just mass ->
            let newTotal = total + fuelRequired mass
            in loop newTotal
          Nothing -> return total

fuelRequired mass = (div mass 3) - 2
