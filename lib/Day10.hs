{-# LANGUAGE TupleSections #-}
module Day10 where

import Data.Function ((&))
import System.IO (isEOF)
import Data.List (sort)
import Data.List.Extra (maximumOn)
import Data.Map (Map)
import Data.Ord (Down(..))
import qualified Data.Map as M

day10_1 :: IO ()
day10_1 = do
  asteroids <- parseInput
  let bestAsteroid = maximumOn (M.size . (flip allLines asteroids)) asteroids
      detectableAsteroids = M.size $ allLines bestAsteroid asteroids
  print detectableAsteroids

day10_2 :: IO ()
day10_2 = do
  asteroids <- parseInput
  let bestAsteroid = maximumOn (M.size . (flip allLines asteroids)) asteroids
      linesWithTargets = allLines bestAsteroid asteroids
      sortedTargets = map (\k -> (head $ linesWithTargets M.! k)) $ sort $ M.keys linesWithTargets
      (x, y) = sortedTargets !! 199
  print $ x * 100 + y

type Asteroid = (Int, Int)
data Change = Positive | Negative
            deriving (Eq, Ord)

data Slope = FiniteSlope Rational
           | InfiniteSlope
           | NegativeInfiniteSlope
           deriving (Eq, Show)

instance Ord Slope where
  compare InfiniteSlope InfiniteSlope = EQ
  compare InfiniteSlope _ = GT
  compare _ InfiniteSlope = LT
  compare NegativeInfiniteSlope NegativeInfiniteSlope = EQ
  compare NegativeInfiniteSlope _ = LT
  compare _ NegativeInfiniteSlope = GT
  compare (FiniteSlope s1) (FiniteSlope s2) = compare s1 s2

data Line = Line { slope :: Slope, dir :: (Change, Change) }
          deriving (Eq)

-- Only used for Ord
data Quadrant = Q1 | Q2 | Q3 | Q4
              deriving Eq

instance Ord Quadrant where
  compare Q1 Q1 = EQ
  compare Q2 Q2 = EQ
  compare Q3 Q3 = EQ
  compare Q4 Q4 = EQ
  compare Q1 _ = LT
  compare _ Q1 = GT
  compare Q2 _ = LT
  compare _ Q2 = GT
  compare Q3 _ = LT
  compare _ Q3 = GT

instance Ord Line where
  compare (Line s1 (cx1, cy1)) (Line s2 (cx2, cy2)) =
    case compare (getQuadrant cx1 cy1) (getQuadrant cx2 cy2) of
      EQ -> compare (Down s1) (Down s2)
      o -> o
    where
      getQuadrant Positive Negative = Q1
      getQuadrant Positive Positive = Q2
      getQuadrant Negative Positive = Q3
      getQuadrant Negative Negative = Q4

allLines :: Asteroid -> [Asteroid] -> Map Line [Asteroid]
allLines a as =
  M.fromListWith (++)
  $ foldr (\b acc ->
              if a == b
              then acc
              else calcLine b:acc
          ) [] as
  where
    calcLine b = (Line (calcSlope a b) (calcDirection a b), [b])

calcSlope :: Asteroid -> Asteroid -> Slope
calcSlope (x1, y1) (x2, y2) =
  if x1 /= x2
  then FiniteSlope $ (fromIntegral $ y1 - y2) / (fromIntegral $ x2 - x1)
  else if y1 >= y2 then InfiniteSlope else NegativeInfiniteSlope

calcDirection :: Asteroid -> Asteroid -> (Change, Change)
calcDirection (x1, y1) (x2, y2) = (calcChange x1 x2, calcChange y1 y2)

calcChange :: Int -> Int -> Change
calcChange p q = if q >= p then Positive else Negative

parseInput :: IO [Asteroid]
parseInput =
  go 0
  where
    go :: Int -> IO [Asteroid]
    go y = do
      eof <- isEOF
      if eof
        then return []
        else do
        line <- getLine
        restOfThem <- go (y+1)
        return $ parseLine y line ++ restOfThem

parseLine :: Int -> String -> [Asteroid]
parseLine y line =
  zip [0..(length line - 1)] line
  & filter ((== '#') . snd)
  & map fst
  & map (,y)
