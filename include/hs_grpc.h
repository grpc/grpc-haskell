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
 
#ifndef HS_HS_GRPC_H
#define HS_HS_GRPC_H

#include <grpc/grpc.h>
#include <grpc/support/time.h>

void hs_grpc_completion_queue_next(grpc_completion_queue *cq,
                                   gpr_timespec *deadline,
                                   grpc_event *out_event);

void hs_grpc_completion_queue_pluck(grpc_completion_queue *cq,
                                    void *tag,
                                    gpr_timespec *deadline,
                                    grpc_event *out_event);

grpc_call *hs_grpc_channel_create_call(grpc_channel *channel,
                                       grpc_call *parent_call,
                                       uint32_t propagation_mask,
                                       grpc_completion_queue *cq,
                                       grpc_slice *method, const grpc_slice *host,
                                       gpr_timespec *deadline);

void hs_grpc_slice_from_copied_buffer(const char *source, size_t length, grpc_slice *out);

void hs_grpc_slice_from_static_string(const char *source, grpc_slice *out);

#endif  /* HS_HS_GRPC_H */
