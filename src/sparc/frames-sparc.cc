// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#if V8_TARGET_ARCH_SPARC

#include "src/assembler.h"
#include "src/frames.h"
#include "src/macro-assembler.h"

#include "src/sparc/assembler-sparc.h"
#include "src/sparc/assembler-sparc-inl.h"
#include "src/sparc/frames-sparc.h"
#include "src/sparc/macro-assembler-sparc.h"

namespace v8 {
namespace internal {

Register JavaScriptFrame::fp_register() {
    UNIMPLEMENTED();
}
Register JavaScriptFrame::context_register() {
    UNIMPLEMENTED();
}

Register JavaScriptFrame::constant_pool_pointer_register() {
  UNIMPLEMENTED();
}


Register StubFailureTrampolineFrame::fp_register() {
    UNIMPLEMENTED();
}
Register StubFailureTrampolineFrame::context_register() {
    UNIMPLEMENTED();
}
Register StubFailureTrampolineFrame::constant_pool_pointer_register() {
    UNIMPLEMENTED();
}

}  // namespace internal
}  // namespace v8

#endif  // V8_TARGET_ARCH_SPARC
