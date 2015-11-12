// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/codegen.h"
#include "src/deoptimizer.h"
#include "src/full-codegen/full-codegen.h"
#include "src/register-configuration.h"
#include "src/safepoint-table.h"

namespace v8 {
namespace internal {

    

int Deoptimizer::patch_size() {
     UNIMPLEMENTED();
}


void Deoptimizer::EnsureRelocSpaceForLazyDeoptimization(Handle<Code> code) {
     UNIMPLEMENTED();
}


void Deoptimizer::PatchCodeForDeoptimization(Isolate* isolate, Code* code) {
     UNIMPLEMENTED();
}


void Deoptimizer::FillInputFrame(Address tos, JavaScriptFrame* frame) {
     UNIMPLEMENTED();
}


void Deoptimizer::SetPlatformCompiledStubRegisters(
    FrameDescription* output_frame, CodeStubDescriptor* descriptor) {
     UNIMPLEMENTED();
}


void Deoptimizer::CopyDoubleRegisters(FrameDescription* output_frame) {
     UNIMPLEMENTED();
}


bool Deoptimizer::HasAlignmentPadding(JSFunction* function) {
     UNIMPLEMENTED();
}


#define __ masm()->


// This code tries to be close to ia32 code so that any changes can be
// easily ported.
void Deoptimizer::TableEntryGenerator::Generate() {
     UNIMPLEMENTED();
}


// Maximum size of a table entry generated below.
const int Deoptimizer::table_entry_size_ = 2 * v8::internal::kInstructionSize;

void Deoptimizer::TableEntryGenerator::GeneratePrologue() {
     UNIMPLEMENTED();
}


void FrameDescription::SetCallerPc(unsigned offset, intptr_t value) {
     UNIMPLEMENTED();
}


void FrameDescription::SetCallerFp(unsigned offset, intptr_t value) {
     UNIMPLEMENTED();
}


void FrameDescription::SetCallerConstantPool(unsigned offset, intptr_t value) {
     UNIMPLEMENTED();
}


#undef __

}  // namespace internal
}  // namespace v8
