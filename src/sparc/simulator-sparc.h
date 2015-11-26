// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.


// Declares a Simulator for SPARC instructions if we are not generating a native
// SPARC binary. This Simulator allows us to run and debug SPARC code generation on
// regular desktop machines.
// V8 calls into generated code by "calling" the CALL_GENERATED_CODE macro,
// which will start execution in the Simulator or forwards to the real entry
// on a SPARC HW platform.

#ifndef V8_SPARC_SIMULATOR_SPARC_H_
#define V8_SPARC_SIMULATOR_SPARC_H_

#include "src/allocation.h"

#if !defined(USE_SIMULATOR)
// Running without a simulator on a native sparc platform.

namespace v8 {
namespace internal {

// When running without a simulator we call the entry directly.
#define CALL_GENERATED_CODE(isolate, entry, p0, p1, p2, p3, p4) \
  (entry(p0, p1, p2, p3, p4))

   
// The stack limit beyond which we will throw stack overflow errors in
// generated code. Because generated code on sparc uses the C stack, we
// just use the C stack limit.
class SimulatorStack : public v8::internal::AllStatic {
 public:
  static inline uintptr_t JsLimitFromCLimit(v8::internal::Isolate* isolate,
                                            uintptr_t c_limit) {
    USE(isolate);
    return c_limit;
  }

  static inline uintptr_t RegisterCTryCatch(v8::internal::Isolate* isolate,
                                            uintptr_t try_catch_address) {
    USE(isolate);
    return try_catch_address;
  }

  static inline void UnregisterCTryCatch(v8::internal::Isolate* isolate) {
    USE(isolate);
  }
};

}  // namespace internal
}  // namespace v8

#else  // !defined(USE_SIMULATOR)
// Running with a simulator.

#include "src/assembler.h"
#include "src/hashmap.h"
#include "src/sparc/constants-sparc.h"

namespace v8 {
namespace internal {
    
class Simulator {

    
};
}  // namespace internal
}  // namespace v8

#endif  // !defined(USE_SIMULATOR)
#endif  // V8_SPARC_SIMULATOR_SPARC_H_
