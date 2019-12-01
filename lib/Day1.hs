module Day1 where

import System.IO.Error
import Control.Exception

day1_1 :: IO ()
day1_1 = do
  total <- calculateFuelUsing fuelRequired
  print total

day1_2 :: IO ()
day1_2 = do
  total <- calculateFuelUsing fuelRequiredWithFuel
  print total

fuelRequired :: Int -> Int
fuelRequired mass = (div mass 3) - 2

fuelRequiredWithFuel :: Int -> Int
fuelRequiredWithFuel mass
  | mass <= 0 = 0
  | otherwise = let fuel = max (fuelRequired mass) 0
                    fuelForFuel = fuelRequiredWithFuel fuel
                in fuel + fuelForFuel

readInt :: IO (Maybe Int)
readInt = catch
          (Just <$> (readLn :: IO Int))
          (\e -> if isEOFError e
                 then return Nothing
                 else throw e)

calculateFuelUsing :: (Int -> Int) -> IO Int
calculateFuelUsing calculator = do
  total <- loop 0
  return total
  where
    loop total = do
      maybeMass <- readInt
      case maybeMass of
        Just mass ->
          let newTotal = total + calculator mass
          in loop newTotal
        Nothing -> return total

