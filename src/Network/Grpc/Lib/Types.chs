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
module Network.Grpc.Lib.Types where

import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr

{#import Network.Grpc.Lib.TimeSpec#}

#include <grpc/grpc.h>
#include "hs_grpc.h"

{#context lib = "grpc" prefix = "grpc"#}

type SizeT = {#type size_t#}

data CChannel
{#pointer *channel as Channel foreign -> CChannel#}

{#pointer *channel_args as ^  newtype#}
data CCompletionQueue
{#pointer *completion_queue as CompletionQueue -> CCompletionQueue#}

data CCall
{#pointer *grpc_call as Call -> CCall#}

{#enum completion_type as ^ {underscoreToCase} add prefix = "Enum" deriving (Show,Eq)#}
{#enum status_code as ^ {underscoreToCase} deriving (Show,Eq)#}
{#enum call_error as ^ {underscoreToCase} deriving (Eq, Show)#}

data Event
  = QueueTimeOut
  | QueueShutdown
  | QueueOpComplete !OpStatus !Tag
    deriving Show
{#pointer *grpc_event as EventPtr -> Event#}

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

type PropagationMask = {#type uint32_t#}

{#fun pure unsafe hs_grpc_default_propagation_mask as defaultPropagationMask
  {} -> `PropagationMask' id #}

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

toStatusCode :: CInt -> StatusCode
toStatusCode = toEnum . fromIntegral

fromStatusCode :: StatusCode -> CInt
fromStatusCode = fromIntegral . fromEnum

toCallError :: CInt -> CallError
toCallError = toEnum . fromIntegral

fromCallError :: CallError -> CInt
fromCallError = fromIntegral . fromEnum
