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
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, ExistentialQuantification, RecordWildCards, NamedFieldPuns #-}
module Network.Grpc.Core.Call
  ( Deadline(..)
  , ClientContext
  , CallOptions
  , withAbsoluteDeadline
  , withRelativeDeadlineSeconds
  , withRelativeDeadlineMillis
  , withParentContext
  , withParentContextPropagating
  , withMetadata
  , withCompression

  , newClientContext
  , destroyClientContext
  , MethodName
  , Arg

  , UnaryResult(..)
  , callUnary
  , callUpstream
  , callDownstream
  , callBidi
  , Client
  , Decoder
  , Encoder
  , Rpc
  , runRpc
  , joinReply

  , RpcReply(..)
  , RpcError(..)
  , RpcStatus(..)

  , recvInitialMetadata
  , waitForStatus
  , sendMessage
  , receiveMessage
  , receiveAllMessages
  , closeCall
  , sendHalfClose
  , cancelCall
  , cancelCallWithStatus
  ) where


import           Control.Concurrent
import           Control.Exception
import           Control.Monad

import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as L
import           Data.Maybe (maybeToList)
import           Data.Monoid ((<>), Last(..))
import           Data.IORef

import           Foreign.C.Types              as C
import qualified Foreign.Marshal.Alloc        as C
import qualified Foreign.Ptr                  as C
import           Foreign.Ptr (Ptr)
import qualified Foreign.ForeignPtr           as C
import qualified Foreign.Storable             as C

-- transformers
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except

import qualified Network.Grpc.CompletionQueue as CQ
import Network.Grpc.Lib.PropagationBits
{#import Network.Grpc.Lib.ByteBuffer#} (Slice, CSlice, ByteBuffer)
import qualified Network.Grpc.Lib.ByteBuffer as BB
{#import Network.Grpc.Lib.Metadata#}
{#import Network.Grpc.Lib.TimeSpec#}
{#import Network.Grpc.Lib.Core#}


#include <grpc/grpc.h>
#include "hs_grpc.h"

{#context lib = "grpc" prefix = "grpc" #}

data Deadline
  = AbsoluteDeadline TimeSpec
  | RelativeDeadline Int -- milliseconds

data ClientContext = ClientContext
  { ccChannel :: Channel
  , ccCQ :: CompletionQueue
  , ccWorker :: CQ.Worker
  }

data CallOptions = CallOptions
  { coDeadline :: Maybe Deadline
  , coParentContext :: Maybe () -- todo
  , coPropagationMask :: Maybe PropagationMask
  , coMetadata :: [Metadata]
  , coCompressionAlgo :: Maybe CompressionAlgorithm
  }

instance Monoid CallOptions where
  mempty = CallOptions Nothing Nothing Nothing [] Nothing
  mappend (CallOptions a b c d e) (CallOptions a' b' c' d' e') =
    CallOptions
      (getLast (Last a <> Last a'))
      (getLast (Last b <> Last b'))
      (getLast (Last c <> Last c'))
      (d <> d')
      (getLast (Last e <> Last e'))

withAbsoluteDeadline :: TimeSpec -> CallOptions
withAbsoluteDeadline deadline =
  mempty { coDeadline = Just (AbsoluteDeadline deadline) }

withRelativeDeadlineSeconds :: Int -> CallOptions
withRelativeDeadlineSeconds seconds =
  mempty { coDeadline = Just (RelativeDeadline (seconds*1000)) }

withRelativeDeadlineMillis :: Int -> CallOptions
withRelativeDeadlineMillis ms =
  mempty { coDeadline = Just (RelativeDeadline ms) }

withParentContext :: () -> CallOptions
withParentContext ctx =
  mempty { coParentContext = Just ctx }

withParentContextPropagating :: () -> PropagationMask -> CallOptions
withParentContextPropagating ctx prop =
  mempty { coParentContext   = Just ctx
         , coPropagationMask = Just prop }

withMetadata :: [Metadata] -> CallOptions
withMetadata md =
  mempty { coMetadata = md }

withCompression :: CompressionAlgorithm -> CallOptions
withCompression algo = mempty { coCompressionAlgo = Just algo }

compressionAsMetadata :: CompressionAlgorithm -> Metadata
compressionAsMetadata algo =
  Metadata
    compressionRequestAlgorithmMdKey
    (compressionAlgorithmName algo)
    0

metadataToSend :: CallOptions -> [Metadata]
metadataToSend co =
  maybeToList (compressionAsMetadata <$> coCompressionAlgo co)
  ++ coMetadata co

resolveDeadline :: CallOptions -> IO TimeSpec
resolveDeadline co =
  case coDeadline co of
    Nothing -> return gprInfFuture
    Just (AbsoluteDeadline deadline) -> return deadline
    Just (RelativeDeadline ms) ->
      millisFromNow (fromIntegral ms)

newClientContext :: Channel -> IO ClientContext
newClientContext chan = do
  cq <- completionQueueCreateForNext reservedPtr
  cqt <- CQ.startCompletionQueueThread cq
  return (ClientContext chan cq cqt)

destroyClientContext :: ClientContext -> IO ()
destroyClientContext ClientContext{ccCQ, ccWorker} = do
  completionQueueShutdown ccCQ
  CQ.waitWorkerTermination ccWorker

type MethodName = Slice
type Arg = B.ByteString

reservedPtr :: Ptr ()
reservedPtr = C.nullPtr

data UnaryResult a = UnaryResult [Metadata] [Metadata] a deriving Show

callUnary :: ClientContext -> CallOptions -> MethodName -> Arg -> IO (RpcReply (UnaryResult L.ByteString))
callUnary ctx@ClientContext{ccChannel, ccCQ} co method arg = do
  deadline <- resolveDeadline co
  bracket (grpcChannelCreateCall (cChannel ccChannel) C.nullPtr propagateDefaults ccCQ method (cHost ccChannel) deadline) grpcCallUnref $ \call0 -> newMVar call0 >>= \mcall -> do
    crw <- newClientReaderWriter ctx mcall

    sendInitOp <- opSendInitialMetadata (metadataToSend co)
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

callDownstream :: ClientContext -> CallOptions -> MethodName -> Arg -> IO (RpcReply (Client B.ByteString L.ByteString))
callDownstream ctx@ClientContext{ccChannel, ccCQ} co method arg = do
  deadline <- resolveDeadline co
  mcall <- grpcChannelCreateCall (cChannel ccChannel) C.nullPtr propagateDefaults ccCQ method (cHost ccChannel) deadline >>= newMVar

  crw <- newClientReaderWriter ctx mcall
  let client = RpcOk (Client crw defaultEncoder defaultDecoder)

  sendInitOp <- opSendInitialMetadata (metadataToSend co)
  sendCloseOp <- opSendCloseFromClient
  sendMessageOp <- opSendMessage arg
  res <- callBatch crw [
        OpX sendInitOp
      , OpX sendCloseOp
      , OpX sendMessageOp
      ]
  case res of
    RpcOk _ -> return client
    RpcError _ -> do
      clientCloseCall crw
      stat <- clientWaitForStatus crw
      case stat of
        RpcOk (RpcStatus _ code msg) -> return (RpcError (StatusError code msg))
        RpcError err -> return (RpcError err)

callUpstream :: ClientContext -> CallOptions -> MethodName -> IO (RpcReply (Client B.ByteString L.ByteString))
callUpstream ctx@ClientContext{ccChannel, ccCQ} co method = do
  deadline <- resolveDeadline co
  mcall <- grpcChannelCreateCall (cChannel ccChannel) C.nullPtr propagateDefaults ccCQ method (cHost ccChannel) deadline >>= newMVar

  crw <- newClientReaderWriter ctx mcall
  let client = RpcOk (Client crw defaultEncoder defaultDecoder)
  sendInitOp <- opSendInitialMetadata (metadataToSend co)
  res <- callBatch crw [ OpX sendInitOp ]

  case res of
    RpcOk _ -> return client
    RpcError _ -> do
      clientCloseCall crw
      stat <- clientWaitForStatus crw
      case stat of
        RpcOk (RpcStatus _ code msg) -> return (RpcError (StatusError code msg))
        RpcError err -> return (RpcError err)

callBidi :: ClientContext -> CallOptions -> MethodName -> IO (RpcReply (Client B.ByteString L.ByteString))
callBidi ctx@ClientContext{ccChannel, ccCQ} co method = do
  deadline <- resolveDeadline co
  mcall <- grpcChannelCreateCall (cChannel ccChannel) C.nullPtr propagateDefaults ccCQ method (cHost ccChannel) deadline >>= newMVar

  crw <- newClientReaderWriter ctx mcall
  let client = Client crw defaultEncoder defaultDecoder
  sendInitOp <- opSendInitialMetadata (metadataToSend co)
  res <- callBatch crw [ OpX sendInitOp ]

  case res of
    RpcOk _ -> return (RpcOk client)
    RpcError _ -> do
      clientCloseCall crw
      stat <- clientWaitForStatus crw
      case stat of
        RpcOk (RpcStatus _ code msg) -> return (RpcError (StatusError code msg))
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

data ClientReaderWriter = ClientReaderWriter {
  context :: ClientContext,
  callMVar_ :: MVar Call,
  initialMDRef :: !(IORef (Maybe [Metadata])),
  trailingMDRef :: !(IORef (Maybe [Metadata])),
  statusFromServer :: !(MVar RpcStatus)
}

newClientReaderWriter :: ClientContext -> MVar Call -> IO ClientReaderWriter
newClientReaderWriter ctx mcall = do
  initMD <- newIORef Nothing
  trailMD <- newIORef Nothing
  status <- newEmptyMVar
  return (ClientReaderWriter ctx mcall initMD trailMD status)

data OpX = forall t. OpX (OpT t)

data OpArray = OpArray { opArrPtr           :: !GrpcOpPtr
                       , opArrLen           :: !CULong
                       , opArrFinishAndFree :: !(IO ())
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
  arr <- toArray ops
  let onBatchComplete = opArrFinishAndFree arr
  CQ.withEvent (ccWorker ctx) onBatchComplete $ \eDesc -> do
    callStatus <- withMVar (callMVar_ crw) $ \call ->
      grpcCallStartBatch call (opArrPtr arr) (opArrLen arr) (CQ.eventTag eDesc) reservedPtr
    case callStatus of
      CallOk -> do
        e <- CQ.interruptibleWaitEvent eDesc
        case e of
          QueueOpComplete OpSuccess _ -> return (RpcOk ())
          QueueOpComplete OpError _ -> return (RpcError (Error "callBatch: op error"))
          QueueTimeOut -> return (RpcError DeadlineExceeded)
          QueueShutdown -> return (RpcError (Error "queue shutdown"))
      _ -> do
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
        {#set grpc_op->data.recv_initial_metadata.recv_initial_metadata#} p arr
      ]
    finish = do
      mds <- readMetadataArray arr
      writeIORef (initialMDRef crw) (Just mds)
      writeIORef value mds
      freeMetadataArray arr
  return (Op add value finish)

opSendMessage :: B.ByteString -> IO (OpT ())
opSendMessage bs = do
  bb <- BB.fromByteString bs
  value <- newIORef ()
  let
    add =
      [ \p -> do
        {#set grpc_op->op#} p (fromIntegral (fromEnum OpSendMessage))
        {#set grpc_op->data.send_message.send_message#} p bb
      ]
    finish =
      BB.byteBufferDestroy bb
  return (Op add value finish)

opRecvMessage :: IO (OpT (Maybe L.ByteString))
opRecvMessage = do
  bbptr <- C.malloc :: IO (Ptr (Ptr BB.CByteBuffer))
  value <- newIORef Nothing
  let
    add =
      [ \p -> do
        {#set grpc_op->op#} p (fromIntegral (fromEnum OpRecvMessage))
        {#set grpc_op->data.recv_message.recv_message#} p bbptr
      ]
    finish = do
      bb <- C.peek bbptr
      C.free bbptr
      writeIORef value =<< if bb /= C.nullPtr
        then do
          lbs <- BB.toLazyByteString bb
          BB.byteBufferDestroy bb
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

opRecvStatusOnClient :: ClientReaderWriter -> IO (OpT RpcStatus)
opRecvStatusOnClient (ClientReaderWriter{..}) = do
  trailingMetadataArrPtr <- mallocMetadataArray
  statusCodePtr <- C.malloc :: IO (Ptr StatusCodeT)
  statusSlice <- BB.mallocSlice
  value <- newIORef (error "opRecvStatusOnClient never ran")
  let
    add =
      [ \p -> do
        {#set grpc_op->op#} p (fromIntegral (fromEnum OpRecvStatusOnClient))
        {#set grpc_op->data.recv_status_on_client.trailing_metadata#} p trailingMetadataArrPtr
        {#set grpc_op->data.recv_status_on_client.status#} p statusCodePtr
        C.withForeignPtr statusSlice $ \statusSlice' -> {#set grpc_op->data.recv_status_on_client.status_details#} p statusSlice'
      ]
    finish = do
      trailingMd <- readMetadataArray trailingMetadataArrPtr
      statusCode <- fmap toStatusCode (C.peek statusCodePtr)
      statusDetails <- BB.toByteString statusSlice
      let status = RpcStatus trailingMd statusCode statusDetails
      writeIORef value status
      putMVar statusFromServer status
      free
    free = do
      freeMetadataArray trailingMetadataArrPtr
      C.free statusCodePtr
      C.finalizeForeignPtr statusSlice
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
clientWaitForStatus crw = do
  prev <- tryReadMVar (statusFromServer crw)
  case prev of
    Just stat -> do
      return (RpcOk stat)
    Nothing -> do
      statusOp <- opRecvStatusOnClient crw
      res <- callBatch crw [ OpX statusOp ]
      case res of
        RpcOk _ -> do
          status <- opRead statusOp
          return (RpcOk status)
        RpcError err ->
          return (RpcError err)

clientWrite :: ClientReaderWriter -> B.ByteString -> IO (RpcReply ())
clientWrite crw@(ClientReaderWriter{..}) arg = do
  sendMessageOp <- opSendMessage arg
  callBatch crw [ OpX sendMessageOp ]

clientSendHalfClose :: ClientReaderWriter -> IO (RpcReply ())
clientSendHalfClose crw@(ClientReaderWriter{..}) = do
  sendCloseOp <- opSendCloseFromClient
  callBatch crw [ OpX sendCloseOp ]

clientCloseCall :: ClientReaderWriter -> IO ()
clientCloseCall crw@ClientReaderWriter{..} = do
  _ <- clientWaitForStatus crw
  modifyMVar_ callMVar_ $ \call -> do
    grpcCallUnref call
    return (error "grpcCallUnref called on this Call")

clientCancelCall :: ClientReaderWriter -> IO (RpcReply ())
clientCancelCall ClientReaderWriter{..} = do
  err <- withMVar callMVar_ $ \call -> do
    grpcCallCancel call reservedPtr
  case err of
    CallOk -> return (RpcOk ())
    _ -> return (RpcError (CallErrorStatus err))

clientCancelCallWithStatus :: ClientReaderWriter -> StatusCode -> B.ByteString -> IO (RpcReply ())
clientCancelCallWithStatus ClientReaderWriter{..} status details = do
  err <- withMVar callMVar_ $ \call -> do
    grpcCallCancelWithStatus call status details reservedPtr
  case err of
    CallOk -> return (RpcOk ())
    _ -> return (RpcError (CallErrorStatus err))

type Rpc a = ExceptT RpcError IO a

joinReply :: RpcReply a -> Rpc a
joinReply (RpcOk a) = return a
joinReply (RpcError err) = throwE err

runRpc :: Rpc a -> IO (RpcReply a)
runRpc m = do
  e <- runExceptT m
  case e of
    Left err -> return (RpcError err)
    Right a -> return (RpcOk a)

clientRWOp :: Client req resp -> (ClientReaderWriter -> IO a) -> Rpc a
clientRWOp client act =
  liftIO (act (clientCrw client))

joinClientRWOp :: Client req resp -> (ClientReaderWriter -> IO (RpcReply a)) -> Rpc a
joinClientRWOp client act = do
  x <- clientRWOp client act
  joinReply x

branchOnClientStatus :: Client req resp
               -> Rpc a
               -> Rpc a
               -> (StatusCode -> B.ByteString -> Rpc a)
               -> Rpc a
branchOnClientStatus client onProcessing onSuccess onFail = do
  status <- clientRWOp client (tryReadMVar . statusFromServer)
  case status of
    Nothing -> onProcessing
    Just (RpcStatus _ code msg)
      | code == StatusOk -> onSuccess
      | otherwise -> onFail code msg

throwIfErrorStatus :: Client req resp -> Rpc ()
throwIfErrorStatus client =
  branchOnClientStatus
    client
    (return ())
    (return ())
    (\code msg -> throwE (StatusError code msg))

recvInitialMetadata :: Client req resp -> Rpc [Metadata]
recvInitialMetadata client = do
  joinClientRWOp client clientWaitForInitialMetadata

waitForStatus :: Client req resp -> Rpc RpcStatus
waitForStatus client = do
  _ <- clientRWOp client clientWaitForInitialMetadata
  joinClientRWOp client clientWaitForStatus

receiveMessage :: Client req resp -> Rpc (Maybe resp)
receiveMessage client = do
  throwIfErrorStatus client
  msg <- joinClientRWOp client clientRead
  case msg of
    Nothing -> return Nothing
    Just x -> do
      let decoder = clientDecoder client
      liftM Just (joinReply =<< liftIO (decoder x))

receiveAllMessages :: Client req resp -> Rpc [resp]
receiveAllMessages client = do
  let
    decoder = clientDecoder client
    go acc = do
        value <- joinClientRWOp client clientRead
        case value of
          Just x -> do
            y <- joinReply =<< (liftIO (decoder x))
            go (y:acc)
          Nothing -> return (reverse acc)
  go []

sendMessage :: Client req resp -> req -> Rpc ()
sendMessage client req = do
  throwIfErrorStatus client
  let encoder = clientEncoder client
  bs <- joinReply =<< liftIO (encoder req)
  joinClientRWOp client (\crw -> clientWrite crw bs)

sendHalfClose :: Client req resp -> Rpc ()
sendHalfClose client = do
  throwIfErrorStatus client
  joinClientRWOp client clientSendHalfClose

closeCall :: Client req resp -> Rpc ()
closeCall client = do
  status <- waitForStatus client
  clientRWOp client clientCloseCall
  case status of
    RpcStatus _ StatusOk _ -> return ()
    RpcStatus _ code detail -> throwE (StatusError code detail)

-- | Called by clients to cancel an RPC on the server.
-- Can be called multiple times, from any thread.
cancelCall :: Client req resp -> Rpc ()
cancelCall client = do
  _ <- clientRWOp client clientCancelCall
  return ()

-- | Called by clients to cancel an RPC on the server.
-- Can be called multiple times, from any thread.
-- If a status has not been received for the call, set it to the status code
-- and description passed in.
-- Importantly, this function does not send status nor description to the
-- remote endpoint.
cancelCallWithStatus :: Client req resp -> StatusCode -> B.ByteString -> Rpc ()
cancelCallWithStatus client status details =
  joinClientRWOp client (\crw -> clientCancelCallWithStatus crw status details)

data RpcStatus = RpcStatus [Metadata] StatusCode B.ByteString
  deriving Show

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
