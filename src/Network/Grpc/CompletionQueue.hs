-- Copyright 2017 gRPC authors.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--------------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
module Network.Grpc.CompletionQueue where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad             (unless, when)

import qualified Data.HashMap.Strict       as Map

import           Foreign.Ptr

import           Network.Grpc.Lib.Core
import           Network.Grpc.Lib.TimeSpec


data Worker = Worker {
  cqEventMap    :: EventMap,
  cqNextEventId :: MVar EventId,
  cqFinished    :: MVar ()
}

type EventId = Int
type Finalizer = IO ()
type EventMap = MVar (Map.HashMap EventId (MVar Event, Finalizer))

data EventDesc = EventDesc (MVar Event) EventId

startCompletionQueueThread :: CompletionQueue -> IO Worker
startCompletionQueueThread cq = do
  eventMap <- newMVar Map.empty
  nextEventId <- newMVar 1
  finish <- newEmptyMVar
  let worker = Worker eventMap nextEventId finish
  _ <- forkIO $ runWorker cq worker
  return worker

waitWorkerTermination :: Worker -> IO ()
waitWorkerTermination w = readMVar (cqFinished w)

eventIdFromTag :: Tag -> EventId
eventIdFromTag tag = tag `minusPtr` nullPtr

runWorker :: CompletionQueue -> Worker -> IO ()
runWorker cq Worker{..} = go
  where
    go = do
      e <- grpcCompletionQueueNext cq gprInfFuture
      case e of
        QueueTimeOut -> return ()
        QueueShutdown -> do
          completionQueueDestroy cq
          b <- tryPutMVar cqFinished ()
          unless b $ putStrLn "** runWorker: error; multiple workers"
        QueueOpComplete _ tag -> do
          mDesc <- modifyMVar cqEventMap $ \eventMap ->
            let mDesc = Map.lookup (eventIdFromTag tag) eventMap
                eventMap' = Map.delete (eventIdFromTag tag) eventMap
            in return (eventMap', mDesc)
          case mDesc of
            Just (mEvent, finalizer) -> do
              exc <- try finalizer
              case exc of
                Left some -> putStrLn ("** runWorker: finalizer threw exception; " ++ show (some :: SomeException))
                Right _ -> return ()
              b <- tryPutMVar mEvent e
              unless b $ putStrLn "** runWorker: I wasn't first"
            Nothing -> putStrLn ("** runWorker: could not find tag = " ++ show (eventIdFromTag tag) ++ ", ignoring")
          go

withEvent :: Worker -> Finalizer -> (EventDesc -> IO a) -> IO a
withEvent worker finish =
  bracket
    (allocateEvent worker finish)
    (releaseEvent worker)

allocateEvent :: Worker -> Finalizer -> IO EventDesc
allocateEvent Worker{..} finish = do
  eventId <- modifyMVar cqNextEventId $ \eventId -> let nextEventId = eventId + 1 in nextEventId `seq` return (nextEventId, eventId)
  eventMVar <- newEmptyMVar
  modifyMVar_ cqEventMap $ \eventMap -> return $! Map.insert eventId (eventMVar, finish) eventMap
  return (EventDesc eventMVar eventId)

releaseEvent :: Worker -> EventDesc -> IO ()
releaseEvent Worker{..} (EventDesc eventMVar eventId) = do
  b <- tryPutMVar eventMVar (error "releaseEvent: unused event cleanup")
  when b $ modifyMVar_ cqEventMap $ \eventMap -> return $! Map.delete eventId eventMap

interruptibleWaitEvent :: EventDesc -> IO Event
interruptibleWaitEvent (EventDesc mvar _) = readMVar mvar

eventTag :: EventDesc -> Tag
eventTag (EventDesc _ eventId) = mkTag eventId
