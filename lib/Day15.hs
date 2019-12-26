{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Day15 where

import IntCode.Read
import IntCode.Types
import IntCode.Execute
import Pipes
import Pipes.Concurrent
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Control.Concurrent.STM.TVar
import Control.Applicative ((<|>))
import Control.Monad (when)

day15_1 :: IO ()
day15_1 = do
  droidCode <- parseMemory
  (secMap, track, _) <- runDroid droidCode
  printMap secMap
  print $ length track

day15_2 :: IO ()
day15_2 = do
  droidCode <- parseMemory
  (secMap, _, oxygenSystem) <- runDroid droidCode
  print $ spreadOxygen oxygenSystem secMap

data DroidCmd = North
              | South
              | West
              | East
              deriving Show

data DroidResponse = HitWall
                   | MoveDone
                   | FoundOxygenSystem
                   deriving Show

data SectionSpace = Wall
                  | Empty
                  | OxygenSystem
                  | Oxygen
                  | Droid
                  | Start
                  deriving (Show, Eq)

type SectionMap = Map (Int, Int) SectionSpace

spreadOxygen :: (Int, Int) -> SectionMap -> Int
spreadOxygen oxygenSystem =
  go [oxygenSystem] . M.insert oxygenSystem Oxygen
  where
    go :: [(Int, Int)] -> SectionMap -> Int
    go lastPlaces theMap
      | emptyCount theMap == 0 = 0
      | otherwise = 1 + uncurry go (oneStep lastPlaces theMap)
    oneStep :: [(Int, Int)] -> SectionMap -> ([(Int, Int)], SectionMap)
    oneStep lastPlaces theMap =
      foldr oneStepFrom ([], theMap) lastPlaces

    oneStepFrom :: (Int, Int) -> ([(Int, Int)], SectionMap) -> ([(Int, Int)], SectionMap)
    oneStepFrom from (nextCoords, theMap) =
      let (north, south, west, east) = nsweCoord from
      in foldr stepTo (nextCoords, theMap) [north, south, west, east]

    stepTo :: (Int, Int) -> ([(Int, Int)], SectionMap) -> ([(Int, Int)], SectionMap)
    stepTo coord (nextCoords, theMap) =
      case M.lookup coord theMap of
        (Just Empty) -> (coord : nextCoords, M.insert coord Oxygen theMap)
        _ -> (nextCoords, theMap)

emptyCount :: SectionMap -> Int
emptyCount theMap = M.size $ M.filter (== Empty) theMap

printMap :: SectionMap -> IO ()
printMap = putStrLn . renderMap

renderMap :: SectionMap -> String
renderMap secMap =
  let maxX = maximum $ map fst $ M.keys secMap
      minX = minimum $ map fst $ M.keys secMap
      maxY = maximum $ map snd $ M.keys secMap
      minY = minimum $ map snd $ M.keys secMap
      mkLine y = [renderPixel $ M.lookup (x,y) secMap | x <- [minX..maxX] ]
      rendered = unlines $ reverse [mkLine y | y <- [minY..maxY]]
  in rendered

renderPixel :: Maybe SectionSpace -> Char
renderPixel Nothing = ' '
renderPixel (Just p) =
  case p of
    Wall -> '#'
    Empty -> '.'
    OxygenSystem -> 'o'
    Oxygen -> 'O'
    Droid -> 'D'
    Start -> 'X'

runDroid :: Code -> IO (SectionMap, [(Int, Int)], (Int, Int))
runDroid droidCode = do
  mapTVar <- newTVarIO M.empty
  trackTVar <- newTVarIO []
  oxygenSystemTVar <- newTVarIO Nothing
  let saveMapFn = atomically . writeTVar mapTVar
      saveTrackFn = atomically . writeTVar trackTVar
      saveOxygenSystem = atomically . writeTVar oxygenSystemTVar . Just
      assembly = interpretDroidResponse
                 >-> droidController saveMapFn saveTrackFn saveOxygenSystem
                 >-> interpretDroidCmd
                 >-> mkExecution droidCode
  runEffect $ loopPipe assembly
  theMap <- readTVarIO mapTVar
  track <- readTVarIO trackTVar
  maybeOxygenSystem <- readTVarIO oxygenSystemTVar
  case maybeOxygenSystem of
    Nothing -> error "No Oxygen System Found!"
    Just oxygenSystem -> return (theMap, track, oxygenSystem)

loopPipe :: MonadIO m => Pipe a a m () -> Effect m ()
loopPipe p = do
  (output, input) <- lift $ liftIO $ spawn unbounded
  fromInput input >-> p >-> toOutput output

interpretDroidCmd :: Functor m => Pipe DroidCmd Int m ()
interpretDroidCmd = do
  cmd <- await
  yield $ case cmd of
            North -> 1
            South -> 2
            West -> 3
            East -> 4
  interpretDroidCmd

interpretDroidResponse :: Functor m => Pipe Signal DroidResponse m ()
interpretDroidResponse = do
  res <- await
  yield $ case res of
            SignalHalted _ -> error "Droid cannot halt!"
            SignalOutput r ->
              case r of
                0 -> HitWall
                1 -> MoveDone
                2 -> FoundOxygenSystem
                _ -> error $ "Invalid Droid Response: " ++ show r
  interpretDroidResponse

data DroidState = DroidState {secMap :: SectionMap, track :: [(Int, Int)]}

droidController :: forall m.Monad m
                => (SectionMap -> m ())
                -> ([(Int, Int)] -> m ())
                -> ((Int, Int) -> m ())
                -> Pipe DroidResponse DroidCmd m ()
droidController saveMap saveTrack saveOxygenSystem = do
  go $ DroidState (M.singleton (0,0) Empty) [(0,0)]
  where
    go :: DroidState -> Pipe DroidResponse DroidCmd m ()
    go DroidState{..} = when (not $ null track) $ do
      let lastPos = head track
      let (cmd, trackAfterMove, stop) =
            case nextMove secMap lastPos of
              Explore exploreCmd ->
                let exploredPos = expectedPosition lastPos exploreCmd
                in (exploreCmd, exploredPos : track, False)
              Backtrack ->
                if null $ tail track
                then (North, track, True)
                else let backtrackPos = head $ tail track
                     in (cmdFor lastPos backtrackPos, tail track, False)
      when (not stop)  $ do
        yield cmd
        response <- await
        (newMap, trackAfterMoveComplete) <-
              case response of
                HitWall ->
                  pure ( M.insert (head trackAfterMove) Wall secMap
                       , tail trackAfterMove)
                MoveDone ->
                  pure ( M.insert (head trackAfterMove) Empty secMap
                       , trackAfterMove )
                FoundOxygenSystem -> do
                  lift $ saveTrack $ tail trackAfterMove
                  lift $ saveOxygenSystem $ head trackAfterMove
                  pure ( M.insert (head trackAfterMove) OxygenSystem secMap
                       , trackAfterMove )
        lift $ saveMap newMap
        go $ DroidState newMap trackAfterMoveComplete

nsweCoord :: (Int, Int) -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int))
nsweCoord (x, y) =
  let north = (x, y + 1)
      south = (x, y - 1)
      west = (x + 1, y)
      east = (x - 1, y)
  in (north, south, west, east)

nswe :: SectionMap -> (Int, Int) -> (Maybe SectionSpace, Maybe SectionSpace, Maybe SectionSpace, Maybe SectionSpace)
nswe secMap (x,y) =
  let (northCoord, southCoord, westCoord, eastCoord) = nsweCoord (x, y)
      north = M.lookup northCoord secMap
      south = M.lookup southCoord secMap
      west = M.lookup westCoord secMap
      east = M.lookup eastCoord secMap
  in (north, south, west, east)

data Move = Explore DroidCmd
          | Backtrack

cmdFor :: (Int, Int) -> (Int, Int) -> DroidCmd
cmdFor (x1, y1) (x2, y2)
  | y2 - y1 == 1 = North
  | y1 - y2 == 1 = South
  | x2 - x1 == 1 = West
  | x1 - x2 == 1 = East
  | otherwise = error $ "Cannot backtrack: " ++ show (x1, y1) ++ " to " ++ show (x2, y2)

nextMove :: SectionMap -> (Int, Int) -> Move
nextMove secMap (x,y) =
  let (north, south, west, east) = nswe secMap (x,y)
      unexploredDir =
        Explore
        <$> ( ifNothing north North
              <|> ifNothing south South
              <|> ifNothing west West
              <|> ifNothing east East)
  in fromMaybe Backtrack unexploredDir

ifNothing :: Maybe a -> b -> Maybe b
ifNothing Nothing b = Just b
ifNothing _ _ = Nothing

ifEmpty :: Maybe SectionSpace -> b -> Maybe b
ifEmpty (Just Empty) b = Just b
ifEmpty _ _ = Nothing

expectedPosition :: (Int, Int) -> DroidCmd -> (Int, Int)
expectedPosition (x, y) cmd =
  case cmd of
    North -> (x, y + 1)
    South -> (x, y - 1)
    West -> (x + 1, y)
    East -> (x - 1, y)
