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
 
void MacroAssembler::set64(int64_t value, Register d, Register tmp) {
  assert_not_delayed();
  v9_dep();

  int hi = (int)(value >> 32);
  int lo = (int)(value & ~0);
  int bits_33to2 = (int)((value >> 2) & ~0);
  // (Matcher::isSimpleConstant64 knows about the following optimizations.)
  if (Assembler::is_simm13(lo) && value == lo) {
    or3(g0, lo, d);
  } else if (hi == 0) {
    Assembler::sethi(lo, d);   // hardware version zero-extends to upper 32
    if (low10(lo) != 0)
      or3(d, low10(lo), d);
  }
  else if ((hi >> 2) == 0) {
    Assembler::sethi(bits_33to2, d);  // hardware version zero-extends to upper 32
    sllx(d, 2, d);
    if (low12(lo) != 0)
      or3(d, low12(lo), d);
  }
  else if (hi == -1) {
    Assembler::sethi(~lo, d);  // hardware version zero-extends to upper 32
    xor3(d, low10(lo) ^ ~low10(~0), d);
  }
  else if (lo == 0) {
    if (Assembler::is_simm13(hi)) {
      or3(g0, hi, d);
    } else {
      Assembler::sethi(hi, d);   // hardware version zero-extends to upper 32
      if (low10(hi) != 0)
        or3(d, low10(hi), d);
    }
    sllx(d, 32, d);
  }
  else {
    Assembler::sethi(hi, tmp);
    Assembler::sethi(lo,   d); // macro assembler version sign-extends
    if (low10(hi) != 0)
      or3 (tmp, low10(hi), tmp);
    if (low10(lo) != 0)
      or3 (  d, low10(lo),   d);
    sllx(tmp, 32, tmp);
    or3 (d, tmp, d);
  }
}
    
    
    
MacroAssembler::MacroAssembler(Isolate* arg_isolate, void* buffer, int size)
    : Assembler(arg_isolate, buffer, size),
      generating_stub_(false),
      has_frame_(false) {
  if (isolate() != NULL) {
    code_object_ =
        Handle<Object>::New(isolate()->heap()->undefined_value(), isolate());
  }
}

// Use the right branch for the platform

void MacroAssembler::br( Condition c, bool a, Predict p, int d ) {
  Assembler::bp(c, a, icc, p, d);
}

void MacroAssembler::br( Condition c, bool a, Predict p, Label* L ) {
  insert_nop_after_cbcond();
  br(c, a, p, branch_offset(L));
}


// Branch that tests either xcc or icc depending on the
// architecture compiled (LP64 or not)
void MacroAssembler::brx( Condition c, bool a, Predict p, int d ) {
#ifdef _LP64
    Assembler::bp(c, a, xcc, p, d);
#else
    MacroAssembler::br(c, a, p, d);
#endif
}

void MacroAssembler::brx( Condition c, bool a, Predict p, Label* L ) {
  insert_nop_after_cbcond();
  brx(c, a, p, branch_offset(L));
}

void MacroAssembler::ba( Label* L ) {
  br(always, false, pt, L);
}

// Warning: V9 only functions
void MacroAssembler::bp( Condition c, bool a, CC cc, Predict p, int d ) {
  Assembler::bp(c, a, cc, p, d);
}

void MacroAssembler::bp( Condition c, bool a, CC cc, Predict p, Label* L ) {
  Assembler::bp(c, a, cc, p, L);
}

void MacroAssembler::fb( FPUCondition c, bool a, Predict p, int d ) {
  fbp(c, a, fcc0, p, d);
}

void MacroAssembler::fb( FPUCondition c, bool a, Predict p, Label* L ) {
  insert_nop_after_cbcond();
  fb(c, a, p, branch_offset(L));
}

void MacroAssembler::fbp( FPUCondition c, bool a, CC cc, Predict p, int d ) {
  Assembler::fbp(c, a, cc, p, d);
}

void MacroAssembler::fbp( FPUCondition c, bool a, CC cc, Predict p, Label* L ) {
  Assembler::fbp(c, a, cc, p, L);
}

// compares (32 bit) register with zero and branches.  NOT FOR USE WITH 64-bit POINTERS
void MacroAssembler::cmp_zero_and_br(Condition c, Register s1, Label* L, bool a, Predict p) {
  tst(s1);
  br (c, a, p, L);
}

// Compares a pointer register with zero and branches on null.
// Does a test & branch on 32-bit systems and a register-branch on 64-bit.
void MacroAssembler::br_null( Register s1, bool a, Predict p, Label* L ) {
  assert_not_delayed();
#ifdef _LP64
  bpr( rc_z, a, p, s1, L );
#else
  tst(s1);
  br ( zero, a, p, L );
#endif
}

void MacroAssembler::br_notnull( Register s1, bool a, Predict p, Label* L ) {
  assert_not_delayed();
#ifdef _LP64
  bpr( rc_nz, a, p, s1, L );
#else
  tst(s1);
  br ( notZero, a, p, L );
#endif
}

// Compare registers and branch with nop in delay slot or cbcond without delay slot.

// Compare integer (32 bit) values (icc only).
void MacroAssembler::cmp_and_br_short(Register s1, Register s2, Condition c,
                                      Predict p, Label* L) {
  assert_not_delayed();
  if (use_cbcond(L)) {
    Assembler::cbcond(c, icc, s1, s2, L);
  } else {
    cmp(s1, s2);
    br(c, false, p, L);
    delayed()->nop();
  }
}

// Compare integer (32 bit) values (icc only).
void MacroAssembler::cmp_and_br_short(Register s1, int simm13a, Condition c,
                                      Predict p, Label* L) {
  assert_not_delayed();
  if (is_simm(simm13a,5) && use_cbcond(L)) {
    Assembler::cbcond(c, icc, s1, simm13a, L);
  } else {
    cmp(s1, simm13a);
    br(c, false, p, L);
    delayed()->nop();
  }
}

// Branch that tests xcc in LP64 and icc in !LP64
void MacroAssembler::cmp_and_brx_short(Register s1, Register s2, Condition c,
                                       Predict p, Label* L) {
  assert_not_delayed();
  if (use_cbcond(L)) {
    Assembler::cbcond(c, ptr_cc, s1, s2, L);
  } else {
    cmp(s1, s2);
    brx(c, false, p, L);
    delayed()->nop();
  }
}

// Branch that tests xcc in LP64 and icc in !LP64
void MacroAssembler::cmp_and_brx_short(Register s1, int simm13a, Condition c,
                                       Predict p, Label* L) {
  assert_not_delayed();
  if (is_simm(simm13a,5) && use_cbcond(L)) {
    Assembler::cbcond(c, ptr_cc, s1, simm13a, L);
  } else {
    cmp(s1, simm13a);
    brx(c, false, p, L);
    delayed()->nop();
  }
}

// Short branch version for compares a pointer with zero.

void MacroAssembler::br_null_short(Register s1, Predict p, Label* L) {
  assert_not_delayed();
  if (use_cbcond(L)) {
    Assembler::cbcond(zero, ptr_cc, s1, 0, L);
    return;
  }
  br_null(s1, false, p, L);
  delayed()->nop();
}

void MacroAssembler::br_notnull_short(Register s1, Predict p, Label* L) {
  assert_not_delayed();
  if (use_cbcond(L)) {
    Assembler::cbcond(notZero, ptr_cc, s1, 0, L);
    return;
  }
  br_notnull(s1, false, p, L);
  delayed()->nop();
}

// Unconditional short branch
void MacroAssembler::ba_short(Label* L) {
  if (use_cbcond(L)) {
    Assembler::cbcond(equal, icc, g0, g0, L);
    return;
  }
  br(always, false, pt, L);
  delayed()->nop();
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
     WARNING("MacroAssembler::TailCallExternalReference");
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

void MacroAssembler::Prologue(bool code_pre_aging) {
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


CodePatcher::CodePatcher(byte* address, int instructions,
                         FlushICache flush_cache)
    : address_(address),
      size_(instructions* kInstructionSize),
      masm_(NULL, address, size_ + Assembler::kGap),
      flush_cache_(flush_cache) {
  // Create a new macro assembler pointing to the address of the code to patch.
  // The size is adjusted with kGap on order for the assembler to generate size
  // bytes of instructions without failing with buffer size constraints.
  DCHECK(masm_.reloc_info_writer.pos() == address_ + size_ + Assembler::kGap);
}


CodePatcher::~CodePatcher() {
  // Indicate that code has changed.
  if (flush_cache_ == FLUSH) {
    Assembler::FlushICacheWithoutIsolate(address_, size_);
  }
  
  WARNING("CodePatcher::~CodePatcher");
/*
  // Check that the code was patched as expected.
  DCHECK(masm_.pc_ == address_ + size_);
  DCHECK(masm_.reloc_info_writer.pos() == address_ + size_ + Assembler::kGap);*/
}


//void CodePatcher::emit_int32(int instr) { masm()->emit_int32(instr); }


}  // namespace internal
}  // namespace v8

#endif  // V8_TARGET_ARCH_SPARC
