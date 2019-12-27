{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}
module Day16 where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Matrix (Matrix)
import qualified Data.Matrix as M

day16_1 :: IO ()
day16_1 = do
  input <- V.fromList <$> readInput
  let output = V.take 8 $ fft 100 input
      outputAsNumber = fromDigits output
  print outputAsNumber

day16_2 :: IO ()
day16_2 = do
  input <- V.fromList <$> readInput
  let offset = fromDigits $ V.take 7 input
      realInput = V.drop offset $ V.foldr (\_ acc -> acc V.++ input) V.empty ([1..10000] :: Vector Int)
      output = V.take 8 $ V.foldr (\_ -> fftTriangular) realInput [1..100]
      outputAsNumber = fromDigits output
  print outputAsNumber

fromDigits :: Vector Int -> Int
fromDigits digits =
  V.foldr (\(n, i) acc -> acc + (ceiling (10 ** i) * n)) 0
  $ V.zip (V.reverse digits) (V.fromList [0..])

fft :: Int -> Vector Int -> Vector Int
fft 0 input = input
fft times input =
  fft (times - 1) $ M.getRow 1 $ ones <$> (M.rowVector input) * (fftPattern $ V.length input)

ones :: Int -> Int
ones n = (abs n) `mod` 10

fftPattern :: Int -> Matrix Int
fftPattern n =
  M.matrix n n fftMultiplier

fftMultiplier :: (Int, Int) -> Int
fftMultiplier (r, c) =
  let i =  (r `div` c) `mod` 4
  in [0,1,0,-1] V.! i

fftTriangular :: Vector Int -> Vector Int
fftTriangular input =
  V.map ones $ V.scanr1 (+) input

readInput :: IO [Int]
readInput = do
  inputStr <- getLine
  return $ map (\c -> read [c]) inputStr

