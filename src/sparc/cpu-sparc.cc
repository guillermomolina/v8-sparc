// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// CPU specific code for sparc independent of OS goes here.

#if V8_TARGET_ARCH_SPARC

#include "src/assembler.h"
#include "src/macro-assembler.h"

namespace v8 {
namespace internal {

void CpuFeatures::FlushICache(void* buffer, size_t size) {
#if !defined(USE_SIMULATOR)
    UNIMPLEMENTED();
#endif  // !USE_SIMULATOR
}

}  // namespace internal
}  // namespace v8

#endif  // V8_TARGET_ARCH_SPARC
