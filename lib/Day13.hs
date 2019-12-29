{-# LANGUAGE OverloadedStrings #-}
module Day13 where

import IntCode.Read
import IntCode.Types
import IntCode.Execute
import Pipes
import Data.Map (Map)
import qualified Data.Map as M
import Control.Concurrent.STM.TVar
import Control.Monad.STM (atomically)
import Data.Text (Text)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T

day13_1 :: IO ()
day13_1 = do
  game <- parseMemory
  (_, rendered) <- runGame emptyInput game
  putStrLn $ "No of blocks: " ++ show (M.size $ M.filter (== Block) rendered)

day13_2 :: IO ()
day13_2 = do
  game <- persistAt 0 2 <$> parseMemory
  (score, rendered) <- runGame smartJoystick game
  putStrLn $ "No of blocks: " ++ show (M.size $ M.filter (== Block) rendered)
  putStrLn $ "Score: " ++ show score

type RenderedTiles = Map (Int, Int) Tile

data Tile = Empty
          | Wall
          | Block
          | HorizontalPaddle
          | Ball
          deriving (Show, Eq)

runGame :: Pipe RenderedTiles Input IO () -> Code -> IO (Int, RenderedTiles)
runGame joystick game = do
  screen <- newTVarIO M.empty :: IO (TVar RenderedTiles)
  scoreTVar <- newTVarIO 0
  let writeFn k v = do
        atomically $ modifyTVar screen (M.insert k v)
        readTVarIO screen >>= renderScreen
      scoreFn = atomically . writeTVar scoreTVar
      screenProducer = do
        lift (readTVarIO screen) >>= yield
        screenProducer
      smartGame = screenProducer
                  >-> joystick
                  >-> mkExecution game
                  >-> renderer writeFn scoreFn
  _ <- runEffect smartGame
  rendered <- readTVarIO screen
  score <- readTVarIO scoreTVar
  return (score, rendered)

renderScreen :: RenderedTiles -> IO ()
renderScreen tiles = do
  let maxX = maximum $ map fst $ M.keys tiles
      maxY = maximum $ map snd $ M.keys tiles
      mkLine y = T.concat [renderPixel $ M.findWithDefault Empty (x,y) tiles|x <- [0..maxX]]
      renderedLines = T.intercalate "\n" [mkLine y|y <- [0..maxY]]
  T.putStrLn renderedLines
  return ()

renderPixel :: Tile -> Text
renderPixel t =
  case t of
    Empty -> "⬜"
    Wall -> "🧱"
    Block -> "⬛"
    HorizontalPaddle -> "🚀"
    Ball -> "🎾"

smartJoystick :: Functor m => Pipe RenderedTiles Input m ()
smartJoystick =
  go Nothing
  where
    go lastBallPos = do
      rendered <- await
      let ballAndPaddle = do
            ball <- findBall rendered
            paddle <- findPaddle rendered
            Just (ball,paddle)
      case ballAndPaddle of
        Nothing -> go lastBallPos
        Just ((ballX, ballY), (paddleX,paddleY)) -> do
          case whereToBe lastBallPos (ballX, ballY) paddleY of
            Nothing -> go $ Just (ballX, ballY)
            Just newPaddleX -> do
              let output = if newPaddleX == paddleX then 0
                           else if newPaddleX > paddleX then 1
                                else (-1)
              yield output
              go $ Just (ballX, ballY)

whereToBe :: Maybe (Int, Int) -> (Int, Int) -> Int -> Maybe Int
whereToBe lastBallPos (bx,by) paddleY =
  case lastBallPos of
    Nothing -> whereToBe' (bx - 1, by - 1) (bx, by) paddleY
    Just (lbx,lby) -> whereToBe' (lbx, lby) (bx, by) paddleY

whereToBe' :: (Int, Int) -> (Int, Int) -> Int -> Maybe Int
whereToBe' (lbx, lby) (bx, by) py =
  if lby > by
  then Just bx
  else Just $ bx + ((bx - lbx) * (py - by - 1))

findBall :: RenderedTiles -> Maybe (Int, Int)
findBall = findPixel Ball

findPaddle :: RenderedTiles -> Maybe (Int, Int)
findPaddle = findPixel HorizontalPaddle

findPixel :: Tile -> RenderedTiles -> Maybe (Int, Int)
findPixel t = listToMaybe . M.foldrWithKey (\k x acc-> if x == t then k:acc else acc ) []

renderer :: (Monad m, MonadFail m, MonadIO m) => ((Int,Int) -> Tile -> m ()) -> (Int -> m ()) -> Consumer Signal m ()
renderer renderFn scoreFn =
  go
  where
    go = do
      sigX <- await
      case sigX of
        SignalHalted _ -> return ()
        SignalOutput x -> do
          sigY <- await
          case sigY of
            SignalHalted _ -> return ()
            SignalOutput y -> do
              case (x,y) of
                (-1, 0) -> do
                  sigScore <- await
                  case sigScore of
                    SignalHalted _ -> return ()
                    SignalOutput score -> do
                      lift $ scoreFn score
                      go
                _ -> do
                  let renderXY = lift . renderFn (x,y)
                  sigTileId <- await
                  case sigTileId of
                    SignalHalted _ -> return ()
                    SignalOutput tileId -> do
                      case tileId of
                        0 -> renderXY Empty
                        1 -> renderXY Wall
                        2 -> renderXY Block
                        3 -> renderXY HorizontalPaddle
                        4 -> renderXY Ball
                        n -> fail $ "Invalid Pixel " ++ show n
                      go
