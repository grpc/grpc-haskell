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
 
#ifndef HS_HS_TIME_H
#define HS_HS_TIME_H

#include <grpc/support/time.h>

void hs_gpr_now(gpr_timespec *result);
void hs_gpr_timespec_free(gpr_timespec *timespec);
gpr_timespec *hs_gpr_time_from_seconds(int64_t x, gpr_timespec *result);
gpr_timespec *hs_gpr_time_from_millis(int64_t x, gpr_timespec *result);
gpr_timespec *hs_gpr_time_add(gpr_timespec *a, gpr_timespec *b, gpr_timespec *result);

void hs_gpr_inf_future(gpr_timespec *result);

#endif  /* HS_HS_TIME_H */
