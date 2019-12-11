{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Day11 where

import Pipes
import Pipes.Concurrent
import Data.Map (Map, singleton, empty, keys, size, findWithDefault, insert)
import IntCode.Types
import IntCode.Execute
import IntCode.Read
import Control.Concurrent.STM.TVar
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

day11_1 :: IO ()
day11_1 = do
  mem <- parseMemory
  print . size =<< runRobotWith Black mem

day11_2 :: IO ()
day11_2 = do
  mem <- parseMemory
  pixels <- runRobotWith White mem
  let lowestY = minimum $ map snd $ keys pixels
      highestY = maximum $ map snd $ keys pixels
      lowestX = minimum $ map fst $ keys pixels
      highestX = maximum $ map fst $ keys pixels
      renderLine y = foldr (\x acc ->
                              renderPixel (findWithDefault Black (x,y) pixels) <>  acc
                           ) "" [lowestX..highestX]
  T.putStrLn $ T.intercalate "\n" $ map renderLine $ reverse [lowestY..highestY]

data Turn = TurnLeft | TurnRight
data Direction = L | R | U | D
data Robot = Robot { at :: (Int, Int), facing :: Direction}
data Colour = Black
            | White
            deriving Show
type Pixels = Map (Int, Int) Colour

runRobotWith :: Colour -> Code -> IO Pixels
runRobotWith startingColour brain = do
  (mailOut, mailIn) <- spawn unbounded
  pic <- newTVarIO empty
  let pipe = fromInput mailIn
              >-> mkExecution brain
              >-> executeRobot startingColour
      withFeedback =
        for pipe
        $ \(painted, lastPixel) -> lift $ do
        atomically $ writeTVar pic painted
        _ <- atomically $ send mailOut lastPixel
        return ()
  runEffect withFeedback
  atomically $ readTVar pic

executeRobot :: Colour -> Pipe Signal (Pixels,Int) IO ()
executeRobot startingColour = do
  go (Robot (0,0) U) (singleton (0,0) startingColour)
  where
    go :: Robot -> Pixels -> Pipe Signal (Pixels,Int) IO ()
    go r@(Robot{..}) painted = do
      yield $ (painted, encodeColour $ findWithDefault Black at painted)
      colourSignal <- await
      case colourSignal of
        SignalHalted _ -> return ()
        SignalOutput colourCode -> do
          let colour = decodeColour colourCode
          turnSignal <- await
          case turnSignal of
            SignalHalted _ -> return ()
            SignalOutput turnCode -> do
              let t = decodeTurn turnCode
                  newRobot = turnAndMove t r
                  newPixels = insert at colour painted
              go newRobot newPixels

    turnAndMove :: Turn -> Robot -> Robot
    turnAndMove t (Robot{..}) =
      let newFacing = (turn t facing)
      in Robot (move newFacing at) newFacing

    turn :: Turn -> Direction -> Direction
    turn TurnLeft L = D
    turn TurnLeft D = R
    turn TurnLeft R = U
    turn TurnLeft U = L
    turn TurnRight L = U
    turn TurnRight U = R
    turn TurnRight R = D
    turn TurnRight D = L

    move :: Direction -> (Int, Int) -> (Int, Int)
    move L (x, y) = (x - 1 , y)
    move R (x, y) = (x + 1 , y)
    move U (x, y) = (x, y + 1)
    move D (x, y) = (x, y - 1)

decodeColour :: Int -> Colour
decodeColour 0 = Black
decodeColour 1 = White
decodeColour _ = error "invalid colour code"

encodeColour :: Colour -> Int
encodeColour Black = 0
encodeColour White = 1

decodeTurn :: Int -> Turn
decodeTurn 0 = TurnLeft
decodeTurn 1 = TurnRight
decodeTurn _ = error "invalid turn code"

renderPixel :: Colour -> Text
renderPixel White = "⬜"
renderPixel Black = "⬛"
