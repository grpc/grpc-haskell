/*
 * Copyright (c) 2016, Google Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of Google Inc. nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL Google Inc. BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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
