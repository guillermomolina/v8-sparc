// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#if V8_TARGET_ARCH_SPARC

#include "src/codegen.h"
#include "src/debug/debug.h"

namespace v8 {
namespace internal {

#define __ ACCESS_MASM(masm)


void EmitDebugBreakSlot(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void DebugCodegen::GenerateSlot(MacroAssembler* masm, RelocInfo::Mode mode,
                                int call_argc) {
    UNIMPLEMENTED();
}


void DebugCodegen::ClearDebugBreakSlot(Address pc) {
    UNIMPLEMENTED();
}


void DebugCodegen::PatchDebugBreakSlot(Address pc, Handle<Code> code) {
    UNIMPLEMENTED();
}


void DebugCodegen::GenerateDebugBreakStub(MacroAssembler* masm,
                                          DebugBreakCallHelperMode mode) {
    WARNING("DebugCodegen::GenerateDebugBreakStub");
}


void DebugCodegen::GeneratePlainReturnLiveEdit(MacroAssembler* masm) {
   WARNING("DebugCodegen::GeneratePlainReturnLiveEdit");
}


void DebugCodegen::GenerateFrameDropperLiveEdit(MacroAssembler* masm) {
   WARNING("DebugCodegen::GenerateFrameDropperLiveEdit");
}


const bool LiveEdit::kFrameDropperSupported = true;

#undef __
}  // namespace internal
}  // namespace v8

#endif  // V8_TARGET_ARCH_SPARC
