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
module Network.Grpc.Lib.TimeSpec where

import Data.Int
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils

import System.IO.Unsafe (unsafePerformIO)

#include <grpc/support/time.h>
#include "hs_grpc.h"
#include "hs_time.h"

{#pointer *gpr_timespec as TimeSpecPtr -> TimeSpec #}

data TimeSpec = TimeSpec {#type int64_t#} {#type int32_t#} {#type gpr_clock_type#} deriving Show

instance Storable TimeSpec where
  sizeOf _ = {#sizeof gpr_timespec#}
  alignment _ = {#alignof gpr_timespec#}
  peek p = do
    tv_sec <- {#get gpr_timespec->tv_sec#} p
    tv_nsec <- {#get gpr_timespec->tv_nsec#} p
    clock_type <- {#get gpr_timespec->clock_type#} p
    return $! TimeSpec tv_sec tv_nsec clock_type
  poke p (TimeSpec tv_sec tv_nsec clock_type) = do
    {#set gpr_timespec->tv_sec#} p tv_sec
    {#set gpr_timespec->tv_nsec#} p tv_nsec
    {#set gpr_timespec->clock_type#} p clock_type

{#fun unsafe hs_gpr_now as gprNow
  { alloca- `TimeSpec' peek* } -> `()' #}

{#fun pure unsafe hs_gpr_time_from_seconds as ^
  { `Int64'
  , alloca- `TimeSpec'} -> `TimeSpec' peek* #}

{#fun pure unsafe hs_gpr_time_from_millis as ^
  { `Int64'
  , alloca- `TimeSpec'} -> `TimeSpec' peek* #}

{#fun pure unsafe hs_gpr_time_add as ^
  { with* `TimeSpec'
  , with* `TimeSpec'
  , alloca- `TimeSpec'} -> `TimeSpec' peek* #}

{#fun unsafe hs_gpr_inf_future as ^
  { alloca- `TimeSpec' peek* } -> `()' #}

secondsFromNow :: Int64 -> IO TimeSpec
secondsFromNow n = do
  now <- gprNow
  return $! now `hsGprTimeAdd` (hsGprTimeFromSeconds n)

millisFromNow :: Int64 -> IO TimeSpec
millisFromNow n = do
  now <- gprNow
  return $! now `hsGprTimeAdd` (hsGprTimeFromMillis n)

gprInfFuture :: TimeSpec
gprInfFuture = unsafePerformIO hsGprInfFuture
