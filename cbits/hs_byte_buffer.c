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
 
#include <stdlib.h>
#include <string.h>

#include <grpc/byte_buffer.h>

grpc_byte_buffer *hs_raw_byte_buffer_create(const char *source, size_t len) {
    grpc_slice slice = grpc_slice_from_copied_buffer(source, len);
    grpc_byte_buffer *bb = grpc_raw_byte_buffer_create(&slice, 1);
    grpc_slice_unref(slice);
    return bb;
}

void hs_grpc_byte_buffer_reader_readall(grpc_byte_buffer_reader *reader, grpc_slice *out_slice) {
	grpc_slice tmp = grpc_byte_buffer_reader_readall(reader);
	memcpy(out_slice, &tmp, sizeof(grpc_slice));
}

void hs_grpc_slice_unref(grpc_slice *slice) {
	grpc_slice_unref(*slice);
}
