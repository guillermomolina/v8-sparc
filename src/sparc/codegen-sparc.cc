// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/sparc/codegen-sparc.h"

#if V8_TARGET_ARCH_SPARC

#include "src/codegen.h"
#include "src/macro-assembler.h"
#include "src/sparc/simulator-sparc.h"

namespace v8 {
namespace internal {

    
#define __ masm.


UnaryMathFunction CreateExpFunction() {
    UNIMPLEMENTED();
}



UnaryMathFunction CreateSqrtFunction() {
#if defined(USE_SIMULATOR)
  return &std::sqrt;
#else
    UNIMPLEMENTED();
#endif
}

#undef __


// -------------------------------------------------------------------------
// Platform-specific RuntimeCallHelper functions.

void StubRuntimeCallHelper::BeforeCall(MacroAssembler* masm) const {
    UNIMPLEMENTED();
}


void StubRuntimeCallHelper::AfterCall(MacroAssembler* masm) const {
    UNIMPLEMENTED();
}


// -------------------------------------------------------------------------
// Code generators

#define __ ACCESS_MASM(masm)

void ElementsTransitionGenerator::GenerateMapChangeElementsTransition(
    MacroAssembler* masm,
    Register receiver,
    Register key,
    Register value,
    Register target_map,
    AllocationSiteMode mode,
    Label* allocation_memento_found) {
    UNIMPLEMENTED();
}


void ElementsTransitionGenerator::GenerateSmiToDouble(
    MacroAssembler* masm,
    Register receiver,
    Register key,
    Register value,
    Register target_map,
    AllocationSiteMode mode,
    Label* fail) {
    UNIMPLEMENTED();
}


void ElementsTransitionGenerator::GenerateDoubleToObject(
    MacroAssembler* masm,
    Register receiver,
    Register key,
    Register value,
    Register target_map,
    AllocationSiteMode mode,
    Label* fail) {
    UNIMPLEMENTED();
}


void StringCharLoadGenerator::Generate(MacroAssembler* masm,
                                       Register string,
                                       Register index,
                                       Register result,
                                       Label* call_runtime) {
    UNIMPLEMENTED();
}

/*
static MemOperand ExpConstant(int index, Register base) {
    UNIMPLEMENTED();
}
*/

void MathExpGenerator::EmitMathExp(MacroAssembler* masm,
                                   DoubleRegister input,
                                   DoubleRegister result,
                                   DoubleRegister double_scratch1,
                                   DoubleRegister double_scratch2,
                                   Register temp1,
                                   Register temp2,
                                   Register temp3) {
    UNIMPLEMENTED();
}

#ifdef DEBUG
// nop(CODE_AGE_MARKER_NOP)
static const uint32_t kCodeAgePatchFirstInstruction = 0x00010180;
#endif


CodeAgingHelper::CodeAgingHelper() {
    UNIMPLEMENTED();
}


#ifdef DEBUG
bool CodeAgingHelper::IsOld(byte* candidate) const {
    UNIMPLEMENTED();
}
#endif


bool Code::IsYoungSequence(Isolate* isolate, byte* sequence) {
    UNIMPLEMENTED();
}


void Code::GetCodeAgeAndParity(Isolate* isolate, byte* sequence, Age* age,
                               MarkingParity* parity) {
    UNIMPLEMENTED();
}


void Code::PatchPlatformCodeAge(Isolate* isolate,
                                byte* sequence,
                                Code::Age age,
                                MarkingParity parity) {
    UNIMPLEMENTED();
}


#undef __

    
}  // namespace internal
}  // namespace v8

#endif  // V8_TARGET_ARCH_SPARC
