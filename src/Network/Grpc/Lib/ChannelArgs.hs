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
{-# LANGUAGE OverloadedStrings #-}

module Network.Grpc.Lib.ChannelArgs
  ( enableSensus
  , disableSensus
  , enableLoadReporting
  , disableLoadReporting
  , maxConcurrentStreams
  , maxReceiveMessageLength
  , maxSendMessageLength
  , http2InitialSequenceNumber
  , http2StreamLookaheadBytes
  , http2HpackTableSizeDecoder
  , http2HpackTableSizeEncoder
  , http2MaxFrameSize
  , defaultAuthority
  , primaryUserAgentString
  , secondaryUserAgentString
  , maxReconnectBackoffMs
  , initialReconnectBackoffMs
  , sslTargetNameOverrideArg
  , maxMetadataSize
  , allowReusePort
  , disableReusePort
  ) where

import qualified Data.ByteString                     as B

import qualified Data.HashMap.Strict                 as Map

import           Network.Grpc.Lib.Core
import           Network.Grpc.Lib.ChannelArgsStrings


-- | Enable census for tracing and stats collection.
enableSensus :: ChannelArgs
enableSensus = argB grpcArg_EnableSensus True

-- | Disable census for tracing and stats collection.
disableSensus :: ChannelArgs
disableSensus = argB grpcArg_EnableSensus False

-- | Enable load reporting.
enableLoadReporting :: ChannelArgs
enableLoadReporting = argB grpcArg_EnableLoadReporting True

-- | Disable load reporting.
disableLoadReporting :: ChannelArgs 
disableLoadReporting = argB grpcArg_EnableLoadReporting False

-- | Maximum number of concurrent incoming streams to allow on a http2
-- connection.
maxConcurrentStreams :: ArgInt -> ChannelArgs
maxConcurrentStreams = argI grpcArg_MaxConcurrentStreams

-- | Maximum message length in bytes that the channel can receive. -1 means
-- unlimited.
maxReceiveMessageLength :: ArgInt -> ChannelArgs
maxReceiveMessageLength = argI grpcArg_MaxReceiveMessageLength

-- | Maximum message length in bytes that the channel can send. -1 means
-- unlimited.
maxSendMessageLength :: ArgInt -> ChannelArgs
maxSendMessageLength = argI grpcArg_MaxSendMessageLength

-- | Initial sequence number for http2 transports.
http2InitialSequenceNumber :: ArgInt -> ChannelArgs
http2InitialSequenceNumber = argI grpcArg_Http2InitialSequenceNumber

-- | Amount of bytes to read ahead on individual streams. Defaults to 64kb,
-- larger values can help throughput on high-latency connections. NOTE: at
-- some point we'd like to auto-tune this, and this parameter will become a
-- no-op.
http2StreamLookaheadBytes :: ArgInt -> ChannelArgs
http2StreamLookaheadBytes = argI grpcArg_Http2StreamLookaheadBytes

-- | How much memory (in bytes) to use for hpack decoding.
http2HpackTableSizeDecoder :: ArgInt -> ChannelArgs
http2HpackTableSizeDecoder = argI grpcArg_Http2HpackTableSizeDecoder

-- | How much memory (in bytes) to use for hpack encoding.
http2HpackTableSizeEncoder :: ArgInt -> ChannelArgs
http2HpackTableSizeEncoder = argI grpcArg_Http2HpackTableSizeEncoder

-- | How big a frame are we willing to receive via HTTP2. Min 16384, max
-- 16777215. Larger values give lower CPU usage for large messages, but more
-- head of line blocking for small messages.
http2MaxFrameSize :: ArgInt -> ChannelArgs
http2MaxFrameSize = argI grpcArg_Http2MaxFrameSize

-- | Default authority to pass if none specified on call construction.
defaultAuthority :: B.ByteString -> ChannelArgs
defaultAuthority = argS grpcArg_DefaultAuthority

-- | Primary user agent: goes at the start of the user-agent metadata sent on
-- each request.
primaryUserAgentString :: B.ByteString -> ChannelArgs
primaryUserAgentString = argS grpcArg_PrimaryUserAgentString

-- | Primary user agent: goes at the end of the user-agent metadata sent on
-- each request.
secondaryUserAgentString :: B.ByteString -> ChannelArgs
secondaryUserAgentString = argS grpcArg_SecondaryUserAgentString

-- | The maximum time between subsequent connection attempts, in ms.
maxReconnectBackoffMs :: ArgInt -> ChannelArgs
maxReconnectBackoffMs = argI grpcArg_MaxReconnectBackoffMs

-- | The time between the first and second connection attempts, in ms.
initialReconnectBackoffMs :: ArgInt -> ChannelArgs
initialReconnectBackoffMs = argI grpcArg_InitialReconnectBackoffMs

-- | The caller of the 'secure_channel_create' functions may override the
-- target name used for SSL host name checking using this channel argument.
-- This should be used for testing only. If this argument is not
-- specified, the name used for SSL host name checking will be the target
-- parameter (assuming that the secure channel is an SSL channel). If this
-- parameter is specified and the underlying is not an SSL channel, it will
-- just be ignored.
sslTargetNameOverrideArg :: B.ByteString -> ChannelArgs
sslTargetNameOverrideArg = argS grpcArg_SslTargetNameOverrideArg

-- | Maximum metadata size, in bytes.
maxMetadataSize :: ArgInt -> ChannelArgs
maxMetadataSize = argI grpcArg_MaxMetadataSize

-- | Allow the use of SO_REUSEPORT if it's available (default allow).
allowReusePort :: ChannelArgs
allowReusePort = argB grpcArg_AllowReuseport True

-- | Disable the use of SO_REUSEPORT (default allow if available).
disableReusePort :: ChannelArgs
disableReusePort = argB grpcArg_AllowReuseport False

arg :: B.ByteString -> ArgValue -> ChannelArgs
arg s v = ChannelArgs (Map.singleton s v)

argB :: B.ByteString -> Bool -> ChannelArgs
argB s = arg s . ArgI . (\x -> if x then 1 else 0)

argI :: B.ByteString -> ArgInt -> ChannelArgs
argI s = arg s . ArgI

argS :: B.ByteString -> B.ByteString -> ChannelArgs
argS s = arg s . ArgS
