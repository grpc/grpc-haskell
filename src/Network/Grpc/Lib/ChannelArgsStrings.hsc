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
module Network.Grpc.Lib.ChannelArgsStrings where

import qualified Data.ByteString as B

#include "grpc/grpc.h"

grpcArg_EnableSensus :: B.ByteString
grpcArg_EnableSensus = (#const_str GRPC_ARG_ENABLE_CENSUS)

grpcArg_EnableLoadReporting :: B.ByteString
grpcArg_EnableLoadReporting = (#const_str GRPC_ARG_ENABLE_LOAD_REPORTING)

grpcArg_MaxConcurrentStreams :: B.ByteString
grpcArg_MaxConcurrentStreams = (#const_str GRPC_ARG_MAX_CONCURRENT_STREAMS)

grpcArg_MaxReceiveMessageLength :: B.ByteString
grpcArg_MaxReceiveMessageLength = (#const_str GRPC_ARG_MAX_RECEIVE_MESSAGE_LENGTH)

grpcArg_MaxSendMessageLength :: B.ByteString
grpcArg_MaxSendMessageLength = (#const_str GRPC_ARG_MAX_SEND_MESSAGE_LENGTH)

grpcArg_Http2InitialSequenceNumber :: B.ByteString
grpcArg_Http2InitialSequenceNumber = (#const_str GRPC_ARG_HTTP2_INITIAL_SEQUENCE_NUMBER)

grpcArg_Http2StreamLookaheadBytes :: B.ByteString
grpcArg_Http2StreamLookaheadBytes = (#const_str GRPC_ARG_HTTP2_STREAM_LOOKAHEAD_BYTES)

grpcArg_Http2HpackTableSizeDecoder :: B.ByteString
grpcArg_Http2HpackTableSizeDecoder = (#const_str GRPC_ARG_HTTP2_HPACK_TABLE_SIZE_DECODER)

grpcArg_Http2HpackTableSizeEncoder :: B.ByteString
grpcArg_Http2HpackTableSizeEncoder = (#const_str GRPC_ARG_HTTP2_HPACK_TABLE_SIZE_ENCODER)

grpcArg_Http2MaxFrameSize :: B.ByteString
grpcArg_Http2MaxFrameSize = (#const_str GRPC_ARG_HTTP2_MAX_FRAME_SIZE)

grpcArg_DefaultAuthority :: B.ByteString
grpcArg_DefaultAuthority = (#const_str GRPC_ARG_DEFAULT_AUTHORITY)

grpcArg_PrimaryUserAgentString :: B.ByteString
grpcArg_PrimaryUserAgentString = (#const_str GRPC_ARG_PRIMARY_USER_AGENT_STRING)

grpcArg_SecondaryUserAgentString :: B.ByteString
grpcArg_SecondaryUserAgentString = (#const_str GRPC_ARG_SECONDARY_USER_AGENT_STRING)

grpcArg_MaxReconnectBackoffMs :: B.ByteString
grpcArg_MaxReconnectBackoffMs = (#const_str GRPC_ARG_MAX_RECONNECT_BACKOFF_MS)

grpcArg_InitialReconnectBackoffMs :: B.ByteString
grpcArg_InitialReconnectBackoffMs = (#const_str GRPC_ARG_INITIAL_RECONNECT_BACKOFF_MS)

grpcArg_SslTargetNameOverrideArg :: B.ByteString
grpcArg_SslTargetNameOverrideArg = (#const_str GRPC_SSL_TARGET_NAME_OVERRIDE_ARG)

grpcArg_MaxMetadataSize :: B.ByteString
grpcArg_MaxMetadataSize = (#const_str GRPC_ARG_MAX_METADATA_SIZE)

grpcArg_AllowReuseport :: B.ByteString
grpcArg_AllowReuseport = (#const_str GRPC_ARG_ALLOW_REUSEPORT)

grpcArg_ResourceQuota :: B.ByteString
grpcArg_ResourceQuota = (#const_str GRPC_ARG_RESOURCE_QUOTA)

grpcArg_ServiceConfig :: B.ByteString
grpcArg_ServiceConfig = (#const_str GRPC_ARG_SERVICE_CONFIG)

grpcArg_LbPolicyName :: B.ByteString
grpcArg_LbPolicyName = (#const_str GRPC_ARG_LB_POLICY_NAME)
