/*
 *
 * Copyright 2017 gRPC authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
 
#ifndef HS_HS_BYTE_BUFFER_H
#define HS_HS_BYTE_BUFFER_H

#include <string.h>
#include <grpc/byte_buffer.h>

grpc_byte_buffer *hs_raw_byte_buffer_create(const char *source, size_t len);

void hs_grpc_byte_buffer_reader_readall(grpc_byte_buffer_reader *reader, grpc_slice *out_slice);

void hs_grpc_slice_unref(grpc_slice *slice);

#endif  /* HS_HS_BYTE_BUFFER_H */
