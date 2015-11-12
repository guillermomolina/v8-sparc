// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#if V8_TARGET_ARCH_SPARC

#include "src/ic/access-compiler.h"

namespace v8 {
namespace internal {

#define __ ACCESS_MASM(masm)


void PropertyAccessCompiler::GenerateTailCall(MacroAssembler* masm,
                                              Handle<Code> code) {
    UNIMPLEMENTED();
}


Register* PropertyAccessCompiler::load_calling_convention() {
    UNIMPLEMENTED();
}


Register* PropertyAccessCompiler::store_calling_convention() {
    UNIMPLEMENTED();
}


#undef __
}  // namespace internal
}  // namespace v8

#endif  // V8_TARGET_ARCH_SPARC
