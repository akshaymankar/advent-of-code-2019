{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Day17 where

import Pipes
import IntCode.Types
import IntCode.Read
import IntCode.Execute
import Data.Char (chr, ord)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Data.List.Split (splitOn)
import Data.List (inits, intersperse)
import Data.List.Extra (firstJust)
import Debug.Trace
import Data.Either (lefts)
import Data.Either.Combinators (rightToMaybe)

day17_1 :: IO ()
day17_1 = do
  cameraCode <- parseMemory
  mapTVar <- newTVarIO M.empty
  runEffect $ emptyInput >-> mkExecution cameraCode >-> interpretAscii >-> drawMap >-> saveToTVar mapTVar
  theMap <- readTVarIO mapTVar
  prettyPrintMap theMap
  print $ sum $ map alignmentParam $ findIntersections theMap

day17_2 :: IO ()
day17_2 = do
  cameraCode <- parseMemory
  mapTVar <- newTVarIO M.empty
  runEffect $ emptyInput >-> mkExecution cameraCode >-> interpretAscii >-> drawMap >-> saveToTVar mapTVar
  theMap <- readTVarIO mapTVar
  runEffect
    $ uncurry robotInput (traceShowId $ split $ path theMap)
    >-> mkExecution (persistAt 0 2 cameraCode)
    >-> printBigOutput

data Pixel = Scaffold
           | Empty
           | Robot RobotFacing
           | EndOfLine
           | StopDrawing
           deriving (Show, Eq)

data RobotFacing = North
                 | South
                 | West
                 | East
                 | Tumbling
                 deriving (Show, Eq)

type SpaceMap = Map (Int, Int) Pixel

fakeInput :: Producer Signal IO ()
fakeInput = do
  textInput <- lift $ getContents
  mapM_ (yield . SignalOutput . ord) textInput
  yield $ SignalHalted $ Memory 0 mempty 0

printBigOutput :: Consumer Signal IO ()
printBigOutput = do
  sig <- await
  case sig of
    SignalHalted _ -> return ()
    SignalOutput out ->
      if out > 127
      then lift $ putStrLn $ "Output: " ++ show out
      else return ()
  printBigOutput

saveToTVar :: TVar a -> Consumer a IO ()
saveToTVar tvar = do
  x <- await
  lift $ atomically $ writeTVar tvar x
  saveToTVar tvar

prettyPrintMap :: SpaceMap -> IO ()
prettyPrintMap theMap =
  let maxX = maximum $ map fst $ M.keys theMap
      maxY = maximum $ map snd $ M.keys theMap
      renderLine y = [renderPixel $ theMap M.! (x, y) | x <- [0..maxX]]
      rendered = unlines $ [renderLine y | y <- [0..maxY]]
  in putStrLn rendered

renderPixel :: Pixel -> Char
renderPixel Scaffold  = '#'
renderPixel Empty     = '.'
renderPixel (Robot f) = renderRobot f
renderPixel _ = error "Not meant to be printed"

renderRobot :: RobotFacing -> Char
renderRobot East     = '>'
renderRobot West     = '<'
renderRobot South    = 'v'
renderRobot North    = '^'
renderRobot Tumbling = 'X'

interpretAscii :: Functor m => Pipe Signal Pixel m ()
interpretAscii = do
  sig <- await
  case sig of
    SignalHalted _ -> yield StopDrawing >> interpretAscii
    SignalOutput asciiCode -> do
      yield $ chrToPixel $ chr asciiCode
      interpretAscii

chrToPixel :: Char -> Pixel
chrToPixel '#'  = Scaffold
chrToPixel '.'  = Empty
chrToPixel '\n' = EndOfLine
chrToPixel '>'  = Robot East
chrToPixel '^'  = Robot North
chrToPixel 'v'  = Robot South
chrToPixel '<'  = Robot West
chrToPixel 'X'  = Robot Tumbling
chrToPixel _    = error "Unknown ASCII Code"

drawMap :: forall m.Functor m => Pipe Pixel SpaceMap m ()
drawMap =
  go (0,0) (M.empty)
  where
    go :: (Int, Int) -> SpaceMap -> Pipe Pixel SpaceMap m ()
    go (x,y) theMap = do
      pixel <- await
      case pixel of
        StopDrawing -> yield theMap
        EndOfLine -> go (0, y + 1) theMap
        p -> go (x + 1, y) $ M.insert (x, y) p theMap

robotInput :: forall m.Functor m => Map RobotFunction [RobotInstruction] -> [RobotFunction] -> Producer Input m ()
robotInput funcDefs mainRoutine = do
  inputList mainRoutine
  inputList $ funcDefs M.! A
  inputList $ funcDefs M.! B
  inputList $ funcDefs M.! C
  yield $ ord 'n'
  yield $ ord '\n'
  where
    inputList :: ToInput a => [a] -> Producer Input m ()
    inputList [] = return ()
    inputList [x] = do
      mapM_ yield $ toInput x
      yield $ ord '\n'
    inputList (x:xs) = do
      mapM_ yield $ toInput x
      yield $ ord ','
      inputList xs

class ToInput a where
  toInput :: a -> [Input]

instance ToInput RobotFunction where
  toInput = map ord . show

instance ToInput RobotInstruction where
  toInput TurnRight = [ord 'R']
  toInput TurnLeft = [ord 'L']
  toInput (Move n) = map ord $ show n

findIntersections :: SpaceMap -> [(Int, Int)]
findIntersections theMap =
  let maxX = maximum $ map fst $ M.keys theMap
      maxY = maximum $ map snd $ M.keys theMap
      hasScaffold coord = case M.lookup coord theMap of
                            Just Scaffold -> True
                            Just (Robot _) -> True
                            _ -> False
      isIntersection (x, y) = all hasScaffold [(x, y), (x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
  in filter isIntersection [(x, y) | x <- [0..maxX], y <- [0..maxY]]

alignmentParam :: (Int, Int) -> Int
alignmentParam = uncurry (*)

data RobotInstruction = Move Int
                      | TurnRight
                      | TurnLeft
                      deriving (Show, Eq)

path :: SpaceMap -> [RobotInstruction]
path theMap =
  let (start, Robot dir) = findRobot theMap
  in go start dir
  where
    go :: (Int, Int) -> RobotFacing -> [RobotInstruction]
    go currentPos dir =
      let turn = findTurn currentPos dir
      in case turn of
           Nothing -> []
           Just (turnInstruction, newDir) ->
             let (moveInstruction, newPos) = move currentPos newDir
             in turnInstruction:moveInstruction:(go newPos newDir)

    pixelAt :: (Int, Int) -> Pixel
    pixelAt coord = M.findWithDefault Empty coord theMap

    towardsRight North = East
    towardsRight South = West
    towardsRight East = South
    towardsRight West = North
    towardsRight Tumbling = error "Tumbling Robot"

    towardsLeft North = West
    towardsLeft South = East
    towardsLeft East = North
    towardsLeft West = South
    towardsLeft Tumbling = error "Tumbling Robot"

    findTurn :: (Int, Int) -> RobotFacing -> Maybe (RobotInstruction, RobotFacing)
    findTurn coord dir =
      let leftPixel = pixelAt (coordTowards (towardsLeft dir) coord)
          rightPixel = pixelAt (coordTowards (towardsRight dir) coord)
      in if leftPixel == Scaffold
         then Just (TurnLeft, towardsLeft dir)
         else if rightPixel == Scaffold
              then Just (TurnRight, towardsRight dir)
              else Nothing

    move :: (Int, Int) -> RobotFacing -> (RobotInstruction, (Int, Int))
    move (x, y) dir =
      let maxX = maximum $ map fst $ M.keys theMap
          maxY = maximum $ map snd $ M.keys theMap
          coordsInLine =
            case dir of
              North -> [(x, newY) | newY <- [y-1, y-2..0]]
              South -> [(x, newY) | newY <- [y+1, y+2..maxY]]
              East -> [(newX, y) | newX <- [x+1,x+2..maxX]]
              West -> [(newX, y) | newX <- [x-1,x-2..0]]
              Tumbling -> error "Tumbling Robot"
          pathSegment = takeWhile (\coord -> pixelAt coord == Scaffold) coordsInLine
      in (Move $ length pathSegment, last pathSegment)

findRobot :: SpaceMap -> ((Int, Int), Pixel)
findRobot = head . M.assocs . (M.filter
                              $ \x ->
                                  case x of
                                    Robot _ -> True
                                    _ -> False
                            )
coordTowards :: RobotFacing -> (Int, Int) -> (Int, Int)
coordTowards North (x, y) = (x, y - 1)
coordTowards South (x, y) = (x, y + 1)
coordTowards West (x, y) = (x - 1, y)
coordTowards East (x, y) = (x + 1, y)
coordTowards Tumbling _ = error "Tumbling Robot"

data RobotFunction = A | B | C
                   deriving (Show, Eq, Ord, Enum, Bounded)

split :: Eq a => [a] -> (Map RobotFunction [a], [RobotFunction])
split xs =
  maybe (error $ "No splits found") id $ split' A [Left xs]

split' :: Eq a => RobotFunction -> [Either [a] RobotFunction] -> Maybe (Map RobotFunction [a], [RobotFunction])
split' funcName instructions =
  let xs = head $ lefts instructions
  in firstJust (findSplitsWith funcName instructions) $ inits xs

findSplitsWith :: Eq a => RobotFunction -> [Either [a] RobotFunction] -> [a] -> Maybe (Map RobotFunction [a], [RobotFunction])
findSplitsWith funcName instructions pat
  | length pat > maxFuncLen = Nothing
  | length pat <= 1 = Nothing
  | otherwise =
      let separate ys = filter (not . null) $ intersperse pat $ splitOn pat ys
          newInstructions = foldr (\x acc ->
                              case x of
                                Left ys ->
                                  let newList = map (\y -> trueToRight (y == pat) y funcName) $ separate ys
                                  in newList ++ acc
                                _ -> x:acc
                          ) [] instructions
      in if funcName /= maxBound
         then do
           (newMap, routine) <- split' (succ funcName) newInstructions
           Just (M.insert funcName pat newMap, routine)
         else do
           routine <- sequence $ map rightToMaybe newInstructions
           Just (M.singleton funcName pat, routine)

trueToRight :: Bool -> a -> b -> Either a b
trueToRight False a _ = Left a
trueToRight True _ b = Right b

maxFuncLen :: Int
maxFuncLen = 10
