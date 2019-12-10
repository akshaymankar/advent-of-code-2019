{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Day7 where

import Data.List (permutations)
import IntCode.Execute
import IntCode.Read
import IntCode.Types
import Pipes
import Pipes.Concurrent hiding (Input, Output)
import Control.Concurrent.STM.TVar (writeTVar)
import qualified Pipes.Concurrent as P

day7_1 :: IO ()
day7_1 = do
  input <- parseMemory
  print $ maximum $ map (calculateThrusterSignal input) (permutations [0..4])

day7_2 :: IO ()
day7_2 = do
  input <- parseMemory
  print =<< maximum <$> mapM (calculateThrusterSignalWithFeedback input) (permutations [5..9])

calculateThrusterSignal :: Code -> [Input] -> Int
calculateThrusterSignal amplifierController settings =
  let execution = mkExecution amplifierController
      [s5,s4,s3,s2,s1] = settings
      in0 = yield $ SignalOutput 0
      in1 = prependProducer s1 (signalToInput in0) >-> execution
      in2 = prependProducer s2 (signalToInput in1) >-> execution
      in3 = prependProducer s3 (signalToInput in2) >-> execution
      in4 = prependProducer s4 (signalToInput in3) >-> execution
      out4 = prependProducer s5 (signalToInput in4) >-> execution
      -- NOTE: Not sure why this doesn't type check
      -- out = foldr (\s p -> prependProducer s (signalToInput p) >-> execution) in0 (reverse settings)
      (_, outputs) = consumeExecution out4
  in case outputs of
       [o] -> o
       os -> error $ "expected only one output, got " ++ show os

calculateThrusterSignalWithFeedback :: Code -> [Input] -> IO Int
calculateThrusterSignalWithFeedback amplifierController [s0,s1,s2,s3,s4] = do
  finalOutput <- newTVarIO minBound
  (mailOut, mailIn) <- spawn unbounded
  let execution = mkExecution amplifierController
      in0 = yield (SignalOutput 0) >> (fromInput mailIn)
      in1 = prependProducer s0 (signalToInput in0) >-> execution
      in2 = prependProducer s1 (signalToInput in1) >-> execution
      in3 = prependProducer s2 (signalToInput in2) >-> execution
      in4 = prependProducer s3 (signalToInput in3) >-> execution
      out4 = prependProducer s4 (signalToInput in4) >-> execution
      final = lastConsumer (atomically . writeTVar finalOutput) mailOut out4
  _ <- consumeExecutionIO final
  atomically $ readTVar finalOutput
calculateThrusterSignalWithFeedback _  _ = error "wrong settings"

lastConsumer :: (Output -> IO ()) -> P.Output Signal -> Producer' Signal IO () -> Producer' Signal IO ()
lastConsumer saveFn mailOut p =
  for p
  $ \case
  signal@(SignalOutput o) -> do
    lift $ saveFn o
    void $ lift $ atomically $ send mailOut signal -- NOTE: This ignores if buffer is exhausted
  SignalHalted _ -> return ()

prependProducer :: Functor m => a -> Producer' a m r -> Producer' a m r
prependProducer x p = yield x >> p

signalToInput :: Functor m => Producer' Signal m r -> Producer' Input m r
signalToInput p =
  for p
  $ \signal ->
      case signal of
        SignalOutput o -> yield o
        _ -> return ()
