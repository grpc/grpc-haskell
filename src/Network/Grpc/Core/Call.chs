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
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, ExistentialQuantification, RecordWildCards #-}
module Network.Grpc.Core.Call where


import           Control.Concurrent
import           Control.Exception
import           Control.Monad

import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as L
import           Data.IORef

import           Foreign.C.Types              as C
import qualified Foreign.ForeignPtr           as C
import qualified Foreign.Marshal.Alloc        as C
import qualified Foreign.Marshal.Utils        as C
import qualified Foreign.Ptr                  as C
import           Foreign.Ptr (Ptr)
import qualified Foreign.Storable             as C

-- transformers
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader

import qualified Network.Grpc.CompletionQueue as CQ
{#import Network.Grpc.Lib.ByteBuffer#}
{#import Network.Grpc.Lib.Metadata#}
{#import Network.Grpc.Lib.TimeSpec#}
{#import Network.Grpc.Lib.Types#}
{#import Network.Grpc.Lib.Grpc#}


#include <grpc/grpc.h>
#include "hs_grpc.h"

{#context lib = "grpc" prefix = "grpc" #}

type Deadline = TimeSpec

data ClientContext = ClientContext
  { ccChannel :: Channel
  , ccCQ :: CompletionQueue
  , ccWorker :: CQ.Worker
  , ccDeadline :: Deadline
  }

emptyChannelArgs :: ChannelArgs
emptyChannelArgs = ChannelArgs C.nullPtr

newClientContext :: Channel -> IO ClientContext
newClientContext chan = do
  cq <- completionQueueCreate reservedPtr
  cqt <- CQ.startCompletionQueueThread cq
  return (ClientContext chan cq cqt gprInfFuture)

destroyClientContext :: ClientContext -> IO ()
destroyClientContext (ClientContext _ cq w _) = do
  completionQueueShutdown cq
  CQ.waitWorkerTermination w

withTimeout :: Deadline -> ClientContext -> ClientContext
withTimeout ts (ClientContext chan cq cqt _) = ClientContext chan cq cqt ts

type MethodName = B.ByteString
type Arg = B.ByteString

-- | Run the IO function once, cache it in the MVar.
onceMVar :: MVar (Maybe a) -> IO a -> IO a
onceMVar mvar io = modifyMVar mvar $ \x ->
  case x of
    Just x' -> return (x, x')
    Nothing -> do
      x' <- io
      return (Just x', x')

reservedPtr :: Ptr ()
reservedPtr = C.nullPtr

callUnary :: ClientContext -> MethodName -> Arg -> [Metadata] -> IO (RpcReply (UnaryResult L.ByteString))
callUnary ctx@(ClientContext chan cq _ deadline) method arg mds =
  C.withForeignPtr chan $ \chanPtr ->
    bracket (grpcChannelCreateCall chanPtr C.nullPtr defaultPropagationMask cq method "localhost" deadline) grpcCallDestroy $ \call0 -> newMVar call0 >>= \mcall -> do
      crw <- newClientReaderWriter ctx mcall

      sendInitOp <- opSendInitialMetadata mds
      sendCloseOp <- opSendCloseFromClient
      sendMessageOp <- opSendMessage arg
      recvStatusOp <- opRecvStatusOnClient crw
      recvMessageOp <- opRecvMessage
      recvInitialMetadataOp <- opRecvInitialMetadata crw

      res <- callBatch crw [
            OpX sendInitOp
          , OpX sendCloseOp
          , OpX recvInitialMetadataOp
          , OpX recvMessageOp
          , OpX sendMessageOp
          , OpX recvStatusOp
          ]
      case res of
        RpcOk _ -> do
          (RpcStatus trailMD status statusDetails) <- opRead recvStatusOp
          case status of
            StatusOk -> do
              initMD <- opRead recvInitialMetadataOp
              answ <- opRead recvMessageOp
              let answ' = maybe L.empty id answ
              return (RpcOk (UnaryResult initMD trailMD answ'))
            _ -> return (RpcError (StatusError status statusDetails))
        RpcError err -> return (RpcError err)

data Client req resp = Client
  { clientCrw :: ClientReaderWriter
  , clientEncoder :: Encoder req
  , clientDecoder :: Decoder resp
  }

type Decoder a = L.ByteString -> IO (RpcReply a)
type Encoder a = a -> IO (RpcReply B.ByteString)

defaultDecoder :: L.ByteString -> IO (RpcReply L.ByteString)
defaultDecoder bs = return (RpcOk bs)

defaultEncoder :: B.ByteString -> IO (RpcReply B.ByteString)
defaultEncoder bs = return (RpcOk bs)

callDownstream :: ClientContext -> MethodName -> Arg -> IO (RpcReply (Client B.ByteString L.ByteString))
callDownstream ctx@(ClientContext chan cq _ deadline) method arg =
  C.withForeignPtr chan $ \chanPtr -> do
    mcall <- grpcChannelCreateCall chanPtr C.nullPtr defaultPropagationMask cq method "localhost" deadline >>= newMVar
    crw <- newClientReaderWriter ctx mcall

    sendInitOp <- opSendInitialMetadata []
    sendCloseOp <- opSendCloseFromClient
    sendMessageOp <- opSendMessage arg

    res <- callBatch crw [
          OpX sendInitOp
        , OpX sendCloseOp
        , OpX sendMessageOp
        ]
    case res of
      RpcOk _ -> do
        return (RpcOk (Client crw defaultEncoder defaultDecoder))
      RpcError err -> return (RpcError err)

callUpstream :: ClientContext -> MethodName -> IO (RpcReply (Client B.ByteString L.ByteString))
callUpstream ctx@(ClientContext chan cq _ deadline) method =
  C.withForeignPtr chan $ \chanPtr -> do
    mcall <- grpcChannelCreateCall chanPtr C.nullPtr defaultPropagationMask cq method "localhost" deadline >>= newMVar
    crw <- newClientReaderWriter ctx mcall

    sendInitOp <- opSendInitialMetadata []

    res <- callBatch crw [ OpX sendInitOp ]
    case res of
      RpcOk _ -> do
        return (RpcOk (Client crw defaultEncoder defaultDecoder))
      RpcError err -> return (RpcError err)

data ClientReaderWriter = ClientReaderWriter {
  context :: ClientContext,
  callMVar_ :: MVar Call,
  initialMDRef :: !(IORef (Maybe [Metadata])),
  trailingMDRef :: !(IORef (Maybe [Metadata])),
  statusFromServer :: !(IORef (Maybe RpcStatus))
}

newClientReaderWriter :: ClientContext -> MVar Call -> IO ClientReaderWriter
newClientReaderWriter ctx mcall = do
  initMD <- newIORef Nothing
  trailMD <- newIORef Nothing
  status <- newIORef Nothing
  return (ClientReaderWriter ctx mcall initMD trailMD status)

data OpX = forall t. OpX (OpT t)

data OpArray = OpArray { opArrPtr :: !GrpcOpPtr
                       , opArrLen :: !CULong
                       , opArrFree :: !(IO ())
                       }

toArray :: [OpX] -> IO OpArray
toArray ops = do
  aptr <- C.mallocBytes (length ops * {#sizeof grpc_op#})
  let ptrs = iterate (`C.plusPtr` {#sizeof grpc_op#}) aptr
      ops' = concatMap (\(OpX op) -> opAdd op) ops
      free = C.free aptr >> sequence_ (map (\(OpX op) -> opFinish op) ops)
      write op p = do
        {#set grpc_op->flags#} p 0
        {#set grpc_op->reserved#} p C.nullPtr
        op p
  zipWithM_ write ops' ptrs
  return $! OpArray aptr (fromIntegral $ length ops) free

callBatch :: ClientReaderWriter -> [OpX] -> IO (RpcReply ())
callBatch crw ops = do
  let ctx = context crw
  CQ.withEvent (ccWorker ctx) $ \eDesc -> do
    arr <- toArray ops
    callStatus <- withMVar (callMVar_ crw) $ \call ->
      grpcCallStartBatch call (opArrPtr arr) (opArrLen arr) (CQ.eventTag eDesc) reservedPtr
    -- print ("callStatus: " ++ show callStatus)
    case callStatus of
      CallOk -> do
        e <- CQ.interruptibleWaitEvent eDesc
        -- print ("event: " ++ show e)
        opArrFree arr -- TODO: We leak if we're interrupted.
        case e of
          QueueOpComplete OpSuccess _ -> return (RpcOk ())
          QueueOpComplete OpError _ -> return (RpcError (Error "callBatch: op error"))
          QueueTimeOut -> return (RpcError DeadlineExceeded)
          QueueShutdown -> return (RpcError (Error "queue shutdown"))
      _ -> do
         -- print callStatus
         return (RpcError (CallErrorStatus callStatus))

data OpT out = Op
  { opAdd    :: [Ptr GrpcOp -> IO ()]
  , opValue  :: IORef out
  , opFinish :: IO ()
  }

opRead :: OpT a -> IO a
opRead op = readIORef (opValue op)

opRecvInitialMetadata :: ClientReaderWriter -> IO (OpT [Metadata])
opRecvInitialMetadata crw = do
  arr <- mallocMetadataArray
  value <- newIORef (error "opRecvInitialMetadata never finished")
  let
    add =
      [ \p -> do
        {#set grpc_op->op#} p (fromIntegral (fromEnum OpRecvInitialMetadata))
        {#set grpc_op->data.recv_initial_metadata#} p arr
      ]
    finish = do
      mds <- readMetadataArray arr
      writeIORef (initialMDRef crw) (Just mds)
      writeIORef value mds
      freeMetadataArray arr
  return (Op add value finish)

opSendMessage :: B.ByteString -> IO (OpT ())
opSendMessage bs = do
  bb <- fromByteString bs
  value <- newIORef ()
  let
    add =
      [ \p -> do
        {#set grpc_op->op#} p (fromIntegral (fromEnum OpSendMessage))
        {#set grpc_op->data.send_message#} p bb
      ]
    finish =
      byteBufferDestroy bb
  return (Op add value finish)

opRecvMessage :: IO (OpT (Maybe L.ByteString))
opRecvMessage = do
  bbptr <- C.malloc :: IO (Ptr (Ptr CByteBuffer))
  value <- newIORef Nothing
  let
    add =
      [ \p -> do
        {#set grpc_op->op#} p (fromIntegral (fromEnum OpRecvMessage))
        {#set grpc_op->data.recv_message#} p bbptr
      ]
    finish = do
      bb <- C.peek bbptr
      C.free bbptr
      writeIORef value =<< if bb /= C.nullPtr
        then do
          lbs <- toLazyByteString bb
          byteBufferDestroy bb
          return (Just lbs)
        else return Nothing
  return (Op add value finish)

opSendInitialMetadata :: [Metadata] -> IO (OpT ())
opSendInitialMetadata elems = do
  (mdArrPtr, free) <- mallocMetadata elems
  value <- newIORef ()
  let
    add =
      [ \p -> do
        {#set grpc_op->op#} p (fromIntegral (fromEnum OpSendInitialMetadata))
        {#set grpc_op->data.send_initial_metadata.count#} p (fromIntegral (length elems))
        {#set grpc_op->data.send_initial_metadata.metadata#} p (C.castPtr mdArrPtr)
        {#set grpc_op->data.send_initial_metadata.maybe_compression_level.is_set#} p 0
        {#set grpc_op->data.send_initial_metadata.maybe_compression_level.level#} p 0
      ]
    finish = do
      free
  return (Op add value finish)

data RpcStatus = RpcStatus [Metadata] StatusCode B.ByteString
  deriving Show

opRecvStatusOnClient :: ClientReaderWriter -> IO (OpT RpcStatus)
opRecvStatusOnClient (ClientReaderWriter{..}) = do
  trailingMetadataArrPtr <- mallocMetadataArray
  statusCodePtr <- C.malloc :: IO (Ptr StatusCodeT)
  ptrPtrStatusStr <- C.new C.nullPtr :: IO (Ptr (Ptr CChar))
  capacityPtr <- C.new 0 :: IO (Ptr SizeT)
  value <- newIORef (error "opRecvStatusOnClient never ran")
  let
    add =
      [ \p -> do
        {#set grpc_op->op#} p (fromIntegral (fromEnum OpRecvStatusOnClient))
        {#set grpc_op->data.recv_status_on_client.trailing_metadata#} p trailingMetadataArrPtr
        {#set grpc_op->data.recv_status_on_client.status#} p statusCodePtr
        {#set grpc_op->data.recv_status_on_client.status_details#} p ptrPtrStatusStr
        {#set grpc_op->data.recv_status_on_client.status_details_capacity#} p capacityPtr
      ]
    finish = do
      trailingMd <- readMetadataArray trailingMetadataArrPtr
      statusCode <- fmap toStatusCode (C.peek statusCodePtr)
      statusDetails <- B.packCString =<< C.peek ptrPtrStatusStr
      let status = RpcStatus trailingMd statusCode statusDetails
      writeIORef value status
      writeIORef statusFromServer (Just status)
      free
    free = do
      freeMetadataArray trailingMetadataArrPtr
      C.free statusCodePtr
      C.free ptrPtrStatusStr
      C.free capacityPtr
  return (Op add value finish)

opSendCloseFromClient :: IO (OpT ())
opSendCloseFromClient = do
  value <- newIORef ()
  let
    add =
      [ \p -> do
        {#set grpc_op->op#} p (fromIntegral (fromEnum OpSendCloseFromClient))
      ]
    finish =
      return ()
  return (Op add value finish)

clientWaitForInitialMetadata :: ClientReaderWriter -> IO (RpcReply [Metadata])
clientWaitForInitialMetadata crw@(ClientReaderWriter { .. }) = do
  initMD <- readIORef initialMDRef
  case initMD of
    Just md -> return (RpcOk md)
    Nothing -> do
      recvInitialMetadataOp <- opRecvInitialMetadata crw
      res <- callBatch crw [ OpX recvInitialMetadataOp ]
      case res of
        RpcOk _ -> do
          md <- opRead recvInitialMetadataOp
          writeIORef initialMDRef (Just md)
          return (RpcOk md)
        RpcError err ->
          return (RpcError err)

clientReadInitialMetadata :: ClientReaderWriter -> IO (RpcReply (Maybe [Metadata]))
clientReadInitialMetadata (ClientReaderWriter {..}) = do
  md <- readIORef initialMDRef
  return (RpcOk md)

clientRead :: ClientReaderWriter -> IO (RpcReply (Maybe L.ByteString))
clientRead crw = do
  _ <- clientWaitForInitialMetadata crw
  recvMessage crw

recvMessage :: ClientReaderWriter -> IO (RpcReply (Maybe L.ByteString))
recvMessage crw = do
  recvMessageOp <- opRecvMessage
  res <- callBatch crw [ OpX recvMessageOp ]
  case res of
    RpcOk _ -> do
      bs <- opRead recvMessageOp
      return (RpcOk bs)
    RpcError err -> do
      putStrLn "recvMessage: callBatch failed"
      return (RpcError err)

clientWaitForStatus :: ClientReaderWriter -> IO (RpcReply RpcStatus)
clientWaitForStatus crw@(ClientReaderWriter{..}) = do
  status <- readIORef statusFromServer
  case status of
    Nothing -> do
      recvStatusOp <- opRecvStatusOnClient crw
      res <- callBatch crw [ OpX recvStatusOp ]
      case res of
        RpcOk _ -> do
          st <- opRead recvStatusOp
          return (RpcOk st)
        RpcError err -> do
          return (RpcError err)
    Just st -> return (RpcOk st)

clientClose :: ClientReaderWriter -> IO ()
clientClose (ClientReaderWriter{..}) = do
  withMVar callMVar_ $ \call ->
    grpcCallDestroy call

clientWrite :: ClientReaderWriter -> B.ByteString -> IO (RpcReply ())
clientWrite crw@(ClientReaderWriter{..}) arg = do
  sendMessageOp <- opSendMessage arg
  callBatch crw [ OpX sendMessageOp ]

clientSendClose :: ClientReaderWriter -> IO (RpcReply ())
clientSendClose crw@(ClientReaderWriter{..}) = do
  sendCloseOp <- opSendCloseFromClient
  callBatch crw [ OpX sendCloseOp ]

clientCloseCall :: ClientReaderWriter -> IO ()
clientCloseCall ClientReaderWriter { callMVar_ = callMVar } =
  withMVar callMVar $ \call ->
  grpcCallDestroy call

type Rpc req resp a = ReaderT (Client req resp) (ExceptT RpcError IO) a

askCrw :: Rpc req resp ClientReaderWriter
askCrw = asks clientCrw

askDecoder :: Rpc req resp (Decoder resp)
askDecoder = asks clientDecoder

askEncoder :: Rpc req resp (Encoder req)
askEncoder = asks clientEncoder

joinReply :: RpcReply a -> Rpc req resp a
joinReply (RpcOk a) = return a
joinReply (RpcError err) = lift (throwE err)

withNewClient :: RpcReply (Client req resp) -> Rpc req resp a -> IO (RpcReply a)
withNewClient r_client m = do
  case r_client of
    RpcOk client -> withClient client m
    RpcError err -> return (RpcError err)

withClient :: Client req resp -> Rpc req resp a -> IO (RpcReply a)
withClient client m = do
  e <- runExceptT (runReaderT m client)
  case e of
    Left err -> return (RpcError err)
    Right a -> return (RpcOk a)

initialMetadata :: Rpc req resp [Metadata]
initialMetadata = do
  crw <- askCrw
  joinReply =<< liftIO (clientWaitForInitialMetadata crw)

waitForStatus :: Rpc req resp RpcStatus
waitForStatus = do
  crw <- askCrw
  joinReply =<< liftIO (clientWaitForStatus crw)

receiveMessage :: Rpc req resp (Maybe resp)
receiveMessage = do
  crw <- askCrw
  msg <- joinReply =<< liftIO (clientRead crw)
  case msg of
    Nothing -> return Nothing
    Just x -> do
      decoder <- askDecoder
      liftM Just (joinReply =<< liftIO (decoder x))

receiveAllMessages :: Rpc req resp [resp]
receiveAllMessages = do
  crw <- askCrw
  decoder <- askDecoder
  let go acc = do
        value <- joinReply =<< liftIO (clientRead crw)
        case value of
          Just x -> do
            y <- joinReply =<< (liftIO (decoder x))
            go (y:acc)
          Nothing -> return (reverse acc)
  go []

sendMessage :: req -> Rpc req resp ()
sendMessage o = do
  crw <- askCrw
  encoder <- askEncoder
  x <- joinReply =<< liftIO (encoder o)
  joinReply =<< liftIO (clientWrite crw x)

sendClose :: Rpc req resp ()
sendClose = do
  crw <- askCrw
  joinReply =<< liftIO (clientSendClose crw)

closeCall :: Rpc req resp ()
closeCall = do
  crw <- askCrw
  liftIO (clientClose crw)

callBidi :: ClientContext -> MethodName -> [Metadata] -> IO (RpcReply (Client B.ByteString L.ByteString))
callBidi ctx@(ClientContext chan cq _ deadline) method mds = do
  C.withForeignPtr chan $ \chanPtr -> do
    mcall <- grpcChannelCreateCall chanPtr C.nullPtr defaultPropagationMask cq method "localhost" deadline >>= newMVar

    crw <- newClientReaderWriter ctx mcall
    sendInitOp <- opSendInitialMetadata mds

    res <- callBatch crw [ OpX sendInitOp ]
    case res of
      RpcOk _ -> return (RpcOk (Client crw defaultEncoder defaultDecoder))
      RpcError err -> return (RpcError err)

data UnaryResult a = UnaryResult [Metadata] [Metadata] a deriving Show

data RpcReply a
  = RpcOk a
  | RpcError RpcError
  deriving Show

data RpcError
  = DeadlineExceeded
  | Cancelled
  | Error String
  | CallErrorStatus CallError
  | StatusError StatusCode B.ByteString
  deriving Show
