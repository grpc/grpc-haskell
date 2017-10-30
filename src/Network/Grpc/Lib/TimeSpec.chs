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
