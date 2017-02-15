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
{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

module Network.Grpc.Lib.Core where

#include <grpc/grpc.h>
#include <grpc/compression.h>
#include "hs_grpc.h"

import Data.List(genericLength)
import Data.Monoid ((<>))
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr (Ptr, nullPtr, castPtr, plusPtr)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.ByteString (ByteString, useAsCString)
import Data.ByteString.Unsafe (unsafePackCString)

import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Data.HashMap.Strict as Map

import Network.Grpc.Lib.PropagationBits
{#import Network.Grpc.Lib.TimeSpec#}
{#import Network.Grpc.Lib.ByteBuffer#}

{#context lib = "grpc" prefix = "grpc"#}

-- grpc_shutdown may block for a long time, should not be marked with unsafe
{#fun unsafe grpc_init as ^ {} -> `()'#}

{#fun grpc_shutdown as ^ {} -> `()'#}


type SizeT = {#type size_t#}

-- --------------------
-- Channel Arguments
-- --------------------

data ChannelArgs = ChannelArgs { unChannelArgs :: Map.HashMap B.ByteString ArgValue }

toList :: ChannelArgs -> [(B.ByteString, ArgValue)]
toList = Map.toList . unChannelArgs

instance Monoid ChannelArgs where
  mempty = ChannelArgs mempty
  mappend (ChannelArgs a) (ChannelArgs b) = ChannelArgs (Map.union b a)

data CChannelArgs
{#pointer *channel_args as GrpcChannelArgs -> CChannelArgs#}

type ArgInt = CInt

data ArgValue
  = ArgI ArgInt
  | ArgS B.ByteString deriving Show

data CArg
{#pointer *arg as GrpcArg -> CArg#}

{#enum arg_type as GrpcArgType {underscoreToCase}#}

withChannelArgs :: ChannelArgs -> (GrpcChannelArgs -> IO a) -> IO a
withChannelArgs (toList -> []) act = act nullPtr
withChannelArgs (toList -> args) act =
  allocaBytes {#sizeof grpc_channel_args#} $ \root -> do
    {#set grpc_channel_args->num_args#} root (genericLength args)
    allocaBytes ({#sizeof grpc_arg#} * length args) $ \arr -> do
      {#set grpc_channel_args->args#} root arr
      let
        write [] _ = act root
        write ((key, value):xs) elemPtr = do
          print (key, value)
          let
            cont = write xs (elemPtr `plusPtr` {#sizeof grpc_arg#})
          B.useAsCString key $ \keyPtr -> do
            {#set grpc_arg->key#} elemPtr keyPtr
            case value of
              ArgI i -> do
                {#set grpc_arg->type#} elemPtr (fromIntegral (fromEnum ArgInteger))
                {#set grpc_arg->value.integer#} elemPtr i
                cont
              ArgS s -> do
                {#set grpc_arg->type#} elemPtr (fromIntegral (fromEnum ArgString))
                B.useAsCString s $ \valuePtr -> do
                  {#set grpc_arg->value.string#} elemPtr valuePtr
                  cont
      write args arr

-- --------------------
-- Channel
-- --------------------

data CChannel
{#pointer *grpc_channel as GrpcChannel foreign -> CChannel#}

{#fun unsafe grpc_insecure_channel_create as ^
  { useAsCString* `ByteString',
    withChannelArgs* `ChannelArgs',
    id `Ptr ()' } -> `GrpcChannel'#}

data Channel = Channel
  { cChannel :: !GrpcChannel
  , cHost    :: !Slice
  }

createInsecureChannel :: B.ByteString -> Int -> ChannelArgs -> IO Channel
createInsecureChannel host port args = do
  chan <- grpcInsecureChannelCreate hostPort args nullPtr
  hostSlice <- sliceFromCopy host
  return $! Channel chan hostSlice
  where
    hostPort = host <> ":" <> (C8.pack (show port))

{#fun unsafe grpc_channel_destroy as ^
  { `GrpcChannel' } -> `()'#}

destroyChannel :: Channel -> IO ()
destroyChannel chan =
  grpcChannelDestroy (cChannel chan)

-- ---------------------------
-- Completion Queue and events
-- ---------------------------

data CCompletionQueue
{#pointer *completion_queue as CompletionQueue -> CCompletionQueue#}

data Event
  = QueueTimeOut
  | QueueShutdown
  | QueueOpComplete !OpStatus !Tag
    deriving Show
{#pointer *grpc_event as EventPtr -> Event#}

{#enum completion_type as ^ {underscoreToCase} add prefix = "Enum" deriving (Show,Eq)#}

{#fun unsafe completion_queue_create as ^
  { `Ptr ()' } -> `CompletionQueue'#}

-- Must be marked with safe (not unsafe) as it may block.
{#fun hs_grpc_completion_queue_next as grpcCompletionQueueNext
  { `CompletionQueue',
    with* `TimeSpec',
    alloca- `Event' peek* } -> `()' #}

-- Must be marked with safe (not unsafe) as it may block.
{#fun hs_grpc_completion_queue_pluck as grpcCompletionQueuePluck
  { `CompletionQueue',
    `Ptr ()',
    with* `TimeSpec',
    alloca- `Event' peek* } -> `()' #}

{#fun unsafe completion_queue_destroy as ^
  {`CompletionQueue'} -> `()'#}

{#fun unsafe completion_queue_shutdown as ^
  {`CompletionQueue'} -> `()'#}

instance Storable Event where
  sizeOf _ = {#sizeof grpc_event#}
  alignment _ = {#alignof grpc_event#}
  peek p
    | p == nullPtr = return QueueTimeOut
    | otherwise = do
        typ <- fmap (toEnum . fromIntegral) $ {#get grpc_event->type#} p
        case typ of
          EnumQueueShutdown -> do
            return QueueShutdown
          EnumQueueTimeout -> do
            return QueueTimeOut
          EnumOpComplete -> do
            success <- {#get grpc_event->success#} p
            tag <- {#get grpc_event->tag#} p
            return $! QueueOpComplete (toOpStatus success) tag
  poke _ _ = error "Storable.Event.poke: Must not call poke on an event"

type Tag = Ptr ()
type StatusCodeT = {#type status_code#}

toOpStatus :: CInt -> OpStatus
toOpStatus 0 = OpError -- non-zero for failure!
toOpStatus _ = OpSuccess

fromOpStatus :: OpStatus -> CInt
fromOpStatus OpError = 0
fromOpStatus _ = 1

{#enum grpc_op_type as OpType {underscoreToCase} deriving (Eq, Show)#}

data OpStatus
  = OpError
  | OpSuccess
  deriving (Enum, Show)

data GrpcOp
{#pointer *grpc_op as GrpcOpPtr -> GrpcOp #}

mkTag :: Int -> Ptr ()
mkTag n = castPtr (nullPtr `plusPtr` n)

-- --------------------
-- Call
-- --------------------

data CCall
{#pointer *grpc_call as Call -> CCall#}

{#enum status_code as ^ {underscoreToCase} deriving (Show,Eq)#}
{#enum call_error as ^ {underscoreToCase} deriving (Eq, Show)#}

toStatusCode :: CInt -> StatusCode
toStatusCode = toEnum . fromIntegral

fromStatusCode :: StatusCode -> CInt
fromStatusCode = fromIntegral . fromEnum

toCallError :: CInt -> CallError
toCallError = toEnum . fromIntegral

fromCallError :: CallError -> CInt
fromCallError = fromIntegral . fromEnum

{#fun unsafe hs_grpc_channel_create_call as grpcChannelCreateCall
  { `GrpcChannel',
    id `Ptr CCall',
    fromIntegral `PropagationMask',
    `CompletionQueue',
    `Slice',
    `Slice',
    with* `TimeSpec' } -> `Call' #}

{#fun unsafe grpc_call_destroy as ^
  {`Call'} -> `()'#}

{#fun unsafe grpc_call_start_batch as ^
  {`Call', `GrpcOpPtr', `CULong', `Ptr ()', `Ptr ()'} -> `CallError' toCallError#}

{#fun unsafe grpc_call_cancel as grpcCallCancel
  { `Call',
    id `Ptr ()' } -> `CallError' #}

{#fun unsafe grpc_call_cancel_with_status as grpcCallCancelWithStatus
  { `Call'
  , `StatusCode'
  , useAsCString* `ByteString'
  , id `Ptr ()' } -> `CallError' #}


-- --------------------
-- Compression
-- --------------------

-- |To be used as initial metadata key for the request of a concrete
-- compression algorithm.
compressionRequestAlgorithmMdKey :: ByteString
compressionRequestAlgorithmMdKey =
  {#const GRPC_COMPRESSION_REQUEST_ALGORITHM_MD_KEY#}

{#enum compression_algorithm as CompressionAlgorithm {underscoreToCase}
  omit (GRPC_COMPRESS_ALGORITHMS_COUNT) #}

{#enum compression_level as CompressionLevel {underscoreToCase}
  omit (GRPC_COMPRESS_LEVEL_COUNT) #}

-- | Updates \a name with the encoding name corresponding to a valid \a
-- algorithm. Note that \a name is statically allocated and must *not* be freed.
-- Returns 1 upon success, 0 otherwise. */
{#fun unsafe grpc_compression_algorithm_name as compressionAlgorithmName_
  {`CompressionAlgorithm'
  , id `Ptr (Ptr CChar)'
  } -> `Bool' #}

-- | Returns the encoding name for the given algorithm.
compressionAlgorithmName :: CompressionAlgorithm -> ByteString
compressionAlgorithmName algo = unsafeDupablePerformIO $
  alloca $ \ptrPtr -> do
    res <- compressionAlgorithmName_ algo ptrPtr
    if res
      then do
        ptr <- peek ptrPtr
        unsafePackCString ptr
      else error "compressionAlgorithmName: no name for algorithm"
