// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// CPU specific code for sparc independent of OS goes here.

#if V8_TARGET_ARCH_SPARC

#include <asm/flush.h>

#include "src/assembler.h"
#include "src/macro-assembler.h"

namespace v8 {
namespace internal {

    // CHECK_NEXT
    // TODO(gmolina): Improve this function
void CpuFeatures::FlushICache(void* buffer, size_t size) {
#if !defined(USE_SIMULATOR)
    WARNING("CpuFeatures::FlushICache - Make this loop all in assembler");
  const int kCacheLineSize = 8;
  intptr_t mask = kCacheLineSize - 1;
  byte *start =
      reinterpret_cast<byte *>(reinterpret_cast<intptr_t>(buffer) & ~mask);
  byte *end = static_cast<byte *>(buffer) + size;
  for (byte *pointer = start; pointer < end; pointer += kCacheLineSize) {
      doflush(pointer);
 /*   __asm__ __volatile__(
        "flush %0  \n"
        : // no output 
        : "r"(pointer));*/
  }
#endif  // !USE_SIMULATOR
}

}  // namespace internal
}  // namespace v8

#endif  // V8_TARGET_ARCH_SPARC
