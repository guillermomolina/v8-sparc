// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include <stdarg.h>
#include <stdlib.h>
#include <cmath>

#if V8_TARGET_ARCH_SPARC

#include "src/assembler.h"
#include "src/base/bits.h"
#include "src/codegen.h"
#include "src/disasm.h"
#include "src/sparc/constants-sparc.h"
#include "src/sparc/frames-sparc.h"
#include "src/sparc/simulator-sparc.h"

#if defined(USE_SIMULATOR)

// Only build the simulator if not compiling for real SPARC hardware.
namespace v8 {
namespace internal {

    
}  // namespace internal
}  // namespace v8

#endif  // USE_SIMULATOR
#endif  // V8_TARGET_ARCH_SPARC
