{-# LANGUAGE LambdaCase #-}
module Day3 where

import Data.Set as Set
import Data.Char
import Data.Foldable (minimumBy)
import Data.Ord (comparing)
import Text.ParserCombinators.ReadP

day3_1 :: IO ()
day3_1 = do
  (wire1,wire2) <- readInput
  let path1 = delete (0,0) $ pathOfWire wire1
  let path2 = delete (0,0) $ pathOfWire wire2
  let intersections = path1 `Set.intersection` path2
  print $ manhattanDistance $ nearestByManhattan intersections

day3_2 :: IO ()
day3_2 = do
  (wire1,wire2) <- readInput
  let path1 = delete (0,0) $ pathOfWire wire1
  let path2 = delete (0,0) $ pathOfWire wire2
  let intersections = path1 `Set.intersection` path2
  print $ totalWalkDistance (wire1, wire2) $ nearestByWalk (wire1, wire2) intersections

data Direction = R | U | L | D
data WireSegment = WireSegment Direction Int
type Wire = [WireSegment]
type Coord = (Int, Int)

nearestByManhattan :: Set Coord -> Coord
nearestByManhattan = minimumBy $ comparing manhattanDistance

nearestByWalk :: (Wire, Wire) -> Set Coord -> Coord
nearestByWalk pair = minimumBy $ comparing $ totalWalkDistance pair

manhattanDistance :: Coord -> Int
manhattanDistance (x, y) = abs x + abs y

totalWalkDistance :: (Wire, Wire) -> Coord -> Int
totalWalkDistance (w1, w2) c = walkUntil c w1 + walkUntil c w2

walkUntil :: Coord -> Wire -> Int
walkUntil =
  go 0 (0,0)
  where
    go :: Int -> Coord -> Coord -> Wire -> Int
    go steps start end =
      \case
        [] -> error "The wire doesn't visit the coord"
        (seg@(WireSegment _ len):segs) ->
          let segmentEnd = endOfSegment start seg
          in case isOnSeg end start seg of
               Nothing -> go (steps + len) segmentEnd end segs
               Just n -> steps + n

isOnSeg :: Coord -> Coord -> WireSegment -> Maybe Int
isOnSeg (x, y) (startX, startY) (WireSegment dir len) =
  case dir of
    R -> if y /= startY
         then Nothing
         else isBetween x startX (startX + len)
    L -> if y /= startY
         then Nothing
         else isBetween x startX (startX - len)
    U -> if x /= startX
         then Nothing
         else isBetween y startY (startY + len)
    D -> if x /= startX
         then Nothing
         else isBetween y startY (startY - len)

isBetween :: Int -> Int -> Int -> Maybe Int
isBetween n start end
  | start > n && n >= end = Just $ start - n
  | start < n && n <= end = Just $ n - start
  | otherwise = Nothing

pathOfWire :: Wire -> Set Coord
pathOfWire =
  go (0,0)
  where
    go :: Coord -> Wire -> Set Coord
    go start =
      \case
        [] -> Set.empty
        seg:segs ->
          let (coords,end) = pathOfSegment start seg
          in coords <> go end segs

pathOfSegment :: Coord -> WireSegment -> (Set Coord, Coord)
pathOfSegment start seg =
  let end = endOfSegment start seg
      points = pointsBetween start end
  in (points, end)

{- Only works correctly when either xs are same or ys are same -}
pointsBetween :: Coord -> Coord -> Set Coord
pointsBetween (startX, startY) (endX, endY) =
  Set.fromList [(x,y) | x <- [(min startX endX)..(max startX endX)]
                      , y <- [(min startY endY)..(max startY endY)]]

endOfSegment :: Coord -> WireSegment -> Coord
endOfSegment (startX, startY) (WireSegment dir len) =
  case dir of
    R -> (startX + len, startY)
    L -> (startX - len, startY)
    U -> (startX, startY + len)
    D -> (startX, startY - len)

readInput :: IO (Wire, Wire)
readInput = do
  w1 <- readWire
  w2 <- readWire
  pure (w1,w2)

readWire :: IO Wire
readWire = do
  inputStr <- getLine
  case readP_to_S wireReadP inputStr of
    [(wire, "")] -> pure wire
    _ -> error $ "Failed to parse '" ++ inputStr ++ "'"

wireReadP :: ReadP Wire
wireReadP =
  sepBy1 wireSegmentReadP commaReadP <* eof

wireSegmentReadP :: ReadP WireSegment
wireSegmentReadP = do
  dir <- directionReadP
  len <- intReadP
  pure $ WireSegment dir len

directionReadP :: ReadP Direction
directionReadP = choice [ decodeChar 'R' R
                        , decodeChar 'U' U
                        , decodeChar 'L' L
                        , decodeChar 'D' D
                        ]

decodeChar :: Char -> a -> ReadP a
decodeChar c x = char c *> pure x

intReadP :: ReadP Int
intReadP = do
  intStr <- many1 $ satisfy isDigit
  pure $ read intStr

commaReadP :: ReadP ()
commaReadP = satisfy (== ',') *> pure ()
