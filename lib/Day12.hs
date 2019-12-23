{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Day12 where

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import qualified Data.Set as S

day12_1 :: IO ()
day12_1 = do
  initialMoons <- sequence $ map (\_ -> readMoon) [1..4]
  let afterSteps = foldr (\_ ms -> step ms) initialMoons [1..1000]
  putStrLn $ "Energy: " ++ show (calculateEnergy afterSteps)

printMoons :: [Moon] -> IO ()
printMoons [] = return ()
printMoons (m:ms) = do
  putStrLn $ show m
  printMoons ms

day12_2 :: IO ()
day12_2 = do
  initialMoons <- sequence $ map (\_ -> readMoon) [1..4]
  let xMoons = map toMoonX initialMoons
      yMoons = map toMoonY initialMoons
      zMoons = map toMoonZ initialMoons
      xSteps = S.size $ go xMoons $ S.fromList []
      ySteps = S.size $ go yMoons $ S.fromList []
      zSteps = S.size $ go zMoons $ S.fromList []
  -- putStrLn $ "Steps X: " ++ show xSteps
  -- putStrLn $ "Steps Y: " ++ show ySteps
  -- putStrLn $ "Steps Z: " ++ show zSteps
  putStrLn $ "Steps: " ++ show (lcm zSteps $ lcm xSteps ySteps)
  where go ms acc = if S.member ms acc
                    then acc
                    else go (stepX ms) $ S.insert ms acc

toMoonX :: Moon -> Moon1D
toMoonX (Moon (x,_,_) (vx,_,_))  = Moon1D x vx

toMoonY :: Moon -> Moon1D
toMoonY (Moon (_,y,_) (_,vy,_))  = Moon1D y vy

toMoonZ :: Moon -> Moon1D
toMoonZ (Moon (_,_,z) (_,_,vz))  = Moon1D z vz

data Moon = Moon {position :: Position, velocity :: Velocity}
          deriving Show

data Moon1D = Moon1D{p :: Int, v :: Int }
           deriving (Show, Eq, Ord)

type Position = (Int, Int, Int)
type Velocity = (Int, Int, Int)

calculateEnergy :: [Moon] -> Int
calculateEnergy =
  sum . map (\(Moon pos vel) ->
               let pot = energy pos
                   kin = energy vel
               in pot * kin)

energy :: (Int, Int, Int) -> Int
energy = triFoldr (+) 0 . triMap abs

step :: [Moon] -> [Moon]
step ms =
  let withGravity = map (applyGravity ms) ms
      withVelocity = map applyVelocity withGravity
  in withVelocity

stepX :: [Moon1D] -> [Moon1D]
stepX ms =
  let withGravity = map (applyGravity1D ms) ms
      withVelocity = map applyVelocity1D withGravity
  in withVelocity

applyGravity :: [Moon] -> Moon -> Moon
applyGravity ms m =
  let velocityDiffs = map (velocityDiff m) ms
      newVelocity = foldr addVelocities (velocity m) velocityDiffs
  in m{velocity = newVelocity}

applyGravity1D :: [Moon1D] -> Moon1D -> Moon1D
applyGravity1D ms m =
  let diff = sum $ map (velocityDiff1D (p m) . p) ms
      newVelocity = v m + diff
  in m{v = newVelocity}

applyVelocity :: Moon -> Moon
applyVelocity m = m{position = triMap2 (+) (position m) (velocity m)}

applyVelocity1D :: Moon1D -> Moon1D
applyVelocity1D m = m{p = p m + v m}

velocityDiff :: Moon -> Moon -> Velocity
velocityDiff (Moon p1 _) (Moon p2 _) =
  triMap2 velocityDiff1D p1 p2

addVelocities :: Velocity -> Velocity -> Velocity
addVelocities = triMap2 (+)

velocityDiff1D :: Int -> Int -> Int
velocityDiff1D x1 x2
  | x1 < x2 = 1
  | x1 > x2 = -1
  | otherwise = 0

triFoldr :: (a -> b -> b) -> b -> (a,a,a) -> b
triFoldr f acc (x,y,z)= foldr f acc [x,y,z]

triMap :: (a -> b) -> (a,a,a) -> (b,b,b)
triMap f (x,y,z) = (f x, f y, f z)

triMap2 :: (a -> b -> c) -> (a,a,a) -> (b,b,b) -> (c,c,c)
triMap2 f (x,y,z) (r,s,t) = (f x r, f y s, f z t)

readMoon :: IO Moon
readMoon = do
  inputStr <- getLine
  case readP_to_S moonReadP inputStr of
    [(moon, "")] -> pure moon
    _ -> error $ "Failed to parse '" ++ inputStr ++ "'"

moonReadP :: ReadP Moon
moonReadP = do
  Moon <$> positionReadP <*> pure (0,0,0)

positionReadP :: ReadP Position
positionReadP = do
  _ <- char '<'
  x <- dimentionReadP 'x'
  _ <- delimiterReadP
  y <- dimentionReadP 'y'
  _ <- delimiterReadP
  z <- dimentionReadP 'z'
  _ <- char '>'
  return (x, y, z)

dimentionReadP :: Char -> ReadP Int
dimentionReadP c = do
  _ <- char c
  _ <- char '='
  intReadP

delimiterReadP :: ReadP ()
delimiterReadP = do
  _ <- char ','
  _ <- char ' '
  return ()

intReadP :: ReadP Int
intReadP = do
  signed <- choice [Just <$> char '-', pure Nothing]
  intStr <- many1 $ satisfy isDigit
  case signed of
    Nothing -> pure $ read intStr
    Just _ -> pure $ read ('-':intStr)
