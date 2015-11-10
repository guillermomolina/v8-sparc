// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include <assert.h>  // For assert
#include <limits.h>  // For LONG_MIN, LONG_MAX.

#if V8_TARGET_ARCH_SPARC

#include "src/base/bits.h"
#include "src/base/division-by-constant.h"
#include "src/bootstrapper.h"
#include "src/codegen.h"
#include "src/debug/debug.h"
#include "src/register-configuration.h"
#include "src/runtime/runtime.h"

#include "src/sparc/macro-assembler-sparc.h"

namespace v8 {
namespace internal {
MacroAssembler::MacroAssembler(Isolate* arg_isolate, void* buffer, int size)
    : Assembler(arg_isolate, buffer, size),
      generating_stub_(false),
      has_frame_(false) {
  if (isolate() != NULL) {
    code_object_ =
        Handle<Object>::New(isolate()->heap()->undefined_value(), isolate());
  }
}

 void MacroAssembler::EnterFrame(StackFrame::Type type,
                                bool load_constant_pool_pointer_reg) {
     UNIMPLEMENTED();
}

int MacroAssembler::LeaveFrame(StackFrame::Type type, int stack_adjustment) {
     UNIMPLEMENTED();
}

void MacroAssembler::TailCallExternalReference(const ExternalReference& ext,
                                               int num_arguments,
                                               int result_size) {
     UNIMPLEMENTED();
}


void MacroAssembler::TailCallRuntime(Runtime::FunctionId fid,
                                     int num_arguments,
                                     int result_size) {
  TailCallExternalReference(ExternalReference(fid, isolate()),
                            num_arguments,
                            result_size);
}


bool MacroAssembler::AllowThisStubCall(CodeStub* stub) {
    UNIMPLEMENTED();
}


// Clobbers object, address, value, and ra, if (ra_status == kRAHasBeenSaved)
// The register 'object' contains a heap object pointer.  The heap object
// tag is shifted away.
void MacroAssembler::RecordWrite(
    Register object,
    Register address,
    Register value,
    RAStatus ra_status,
    SaveFPRegsMode fp_mode,
    RememberedSetAction remembered_set_action,
    SmiCheck smi_check,
    PointersToHereCheck pointers_to_here_check_for_value) {
    UNIMPLEMENTED();
}

}  // namespace internal
}  // namespace v8

#endif  // V8_TARGET_ARCH_SPARC
