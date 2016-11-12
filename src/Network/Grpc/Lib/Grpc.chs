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
module Network.Grpc.Lib.Grpc where

import Data.ByteString (ByteString, useAsCString)

import Foreign.Ptr (Ptr, nullPtr, castPtr, plusPtr)
import Foreign.C.Types
import Foreign.Marshal.Utils

import qualified Data.ByteString as B

import Network.Grpc.Lib.PropagationBits
{#import Network.Grpc.Lib.Types#}
{#import Network.Grpc.Lib.TimeSpec#}

#include <grpc/grpc.h>
#include "hs_grpc.h"

-- grpc_shutdown may block for a long time, should not be marked with unsafe
{#fun unsafe grpc_init as ^ {} -> `()'#}

{#fun grpc_shutdown as ^ {} -> `()'#}

{#fun unsafe grpc_insecure_channel_create as ^
  { useAsCString* `ByteString',
    withChannelArgs* `ChannelArgs',
    id `Ptr ()' } -> `Channel'#}

createInsecureChannel :: B.ByteString -> ChannelArgs -> IO Channel
createInsecureChannel hostPort args =
    grpcInsecureChannelCreate hostPort args nullPtr

{#fun unsafe grpc_channel_destroy as ^
  { `Channel' } -> `()'#}

{#fun unsafe hs_grpc_channel_create_call as grpcChannelCreateCall
  { id `Ptr CChannel',
    id `Ptr CCall',
    fromIntegral `PropagationMask',
    `CompletionQueue',
    useAsCString* `ByteString',
    useAsCString* `ByteString',
    with* `TimeSpec' } -> `Call' #}

{#fun unsafe grpc_call_destroy as ^
  {`Call'} -> `()'#}

{#fun unsafe grpc_call_start_batch as ^
  {`Call', `GrpcOpPtr', `CULong', `Ptr ()', `Ptr ()'} -> `CallError' toCallError#}

{#fun unsafe grpc_call_cancel as ^
  { `Call',
    id `Ptr ()' } -> `CallError' #}

{#fun unsafe grpc_call_cancel_with_status as ^
  { `Call'
  , `StatusCode'
  , useAsCString* `ByteString'
  , id `Ptr ()' } -> `CallError' #}

mkTag :: Int -> Ptr ()
mkTag n = castPtr (nullPtr `plusPtr` n)

