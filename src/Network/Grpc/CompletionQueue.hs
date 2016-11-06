-- Copyright (c) 2016, Google Inc.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of Google Inc. nor the
--       names of its contributors may be used to endorse or promote products
--       derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL Google Inc. BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
--------------------------------------------------------------------------------
module Network.Grpc.CompletionQueue where


import           Control.Concurrent
import           Control.Exception
import           Control.Monad                (when, unless)

import qualified Data.HashMap.Strict          as Map

import           Foreign.Ptr

import           Network.Grpc.Lib.TimeSpec
import           Network.Grpc.Lib.Grpc
import           Network.Grpc.Lib.Types


data Worker = Worker {
  cqEventMap :: EventMap,
  cqNextEventId :: MVar EventId,
  cqFinished :: MVar ()
}

type EventId = Int
type EventMap = MVar (Map.HashMap EventId (MVar Event))

data EventDesc = EventDesc (MVar Event) EventId

startCompletionQueueThread :: CompletionQueue -> IO Worker
startCompletionQueueThread cq = do
  eventMap <- newMVar Map.empty
  nextEventId <- newMVar 0
  finish <- newEmptyMVar
  let worker = Worker eventMap nextEventId finish
  _ <- forkIO $ runWorker cq worker
  return worker

waitWorkerTermination :: Worker -> IO ()
waitWorkerTermination w = readMVar (cqFinished w)

eventIdFromTag :: Tag -> EventId
eventIdFromTag tag = tag `minusPtr` nullPtr

runWorker :: CompletionQueue -> Worker -> IO ()
runWorker cq (Worker { cqEventMap = emap, cqFinished = signalFinished }) = go
  where
    go = do
      -- putStrLn "** runWorker: blocking for next event"
      e <- grpcCompletionQueueNext cq gprInfFuture
      -- putStrLn "** runWorker: got event, handling"
      -- print e
      case e of
        QueueTimeOut -> return ()
        QueueShutdown -> do
          completionQueueDestroy cq
          -- putStrLn "** runWorker exiting"
          b <- tryPutMVar signalFinished ()
          unless b $ putStrLn "** runWorker: error; multiple workers"
        QueueOpComplete _ tag -> do
          mvar <- modifyMVar emap $ \emap' ->
            let mvar = Map.lookup (eventIdFromTag tag) emap'
                emap'' = Map.delete (eventIdFromTag tag) emap'
            in return (emap'', mvar)
          case mvar of
            Just mvar' -> do
              b <- tryPutMVar mvar' e
              unless b $ putStrLn "** runWorker: I wasn't first"
            Nothing -> putStrLn ("** runWorker: could not find tag = " ++ show (eventIdFromTag tag) ++ ", ignoring")
          go

withEvent :: Worker -> (EventDesc -> IO a) -> IO a
withEvent (Worker { cqEventMap = emap, cqNextEventId = nextEventIdMVar }) = bracket aquire release
  where
    aquire = do
      eventId <- modifyMVar nextEventIdMVar $ \eventId -> let nextEventId = eventId + 1 in nextEventId `seq` return (nextEventId, eventId)
      eventMVar <- newEmptyMVar
      modifyMVar_ emap $ \emap' -> return $! Map.insert eventId eventMVar emap'
      -- putStrLn $ "** expecting eventId = " ++ show eventId
      return (EventDesc eventMVar eventId)
    release (EventDesc eventMVar eventId) = do
      -- putStrLn $ "** no longer expecting eventId = " ++ show eventId
      b <- tryPutMVar eventMVar (error "withEvent: unused event cleanup")
      when b $ modifyMVar_ emap $ \emap' -> return $! Map.delete eventId emap'

interruptibleWaitEvent :: EventDesc -> IO Event
interruptibleWaitEvent (EventDesc mvar _) = readMVar mvar

eventTag :: EventDesc -> Tag
eventTag (EventDesc _ eventId) = mkTag eventId
