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
#include "src/sparc/macro-assembler-sparc-inl.h"

namespace v8 {
namespace internal {

    
// -----------------------------------------------------------------------------
// Implementation of Operand and MemOperand.
// See assembler-mips-inl.h for inlined constructors.

Operand::Operand(Handle<Object> handle) {
  AllowDeferredHandleDereference using_raw_address;
  rm_ = no_reg;
  // Verify all Objects referred by code are NOT in new space.
  Object* obj = *handle;
  if (obj->IsHeapObject()) {
    DCHECK(!HeapObject::cast(obj)->GetHeap()->InNewSpace(obj));
    imm64_ = reinterpret_cast<intptr_t>(handle.location());
    rmode_ = RelocInfo::EMBEDDED_OBJECT;
  } else {
    // No relocation needed.
    imm64_ = reinterpret_cast<intptr_t>(obj);
    rmode_ = RelocInfo::NONE64;
  }
}


bool Operand::must_output_reloc_info(const MacroAssembler* masm) const {
  if (rmode_ == RelocInfo::EXTERNAL_REFERENCE) {
    if (masm != NULL && masm->predictable_code_size()) return true;
    return masm->serializer_enabled();
  } else if (RelocInfo::IsNone(rmode_)) {
    return false;
  }
  return true;
}

MemOperand Argument::address_in_frame() const {
  // Warning: In LP64 mode disp will occupy more than 10 bits, but
  //          op codes such as ld or ldx, only access disp() to get
  //          their simm13 argument.
  int disp = ((_number - Argument::n_register_parameters + FrameConstants::memory_parameter_word_sp_offset) * kWordSize) + kStackBias;
  if (is_in())
    return MemOperand(fp, disp); // In argument.
  else
    return MemOperand(sp, disp); // Out argument.
}

MacroAssembler::MacroAssembler(Isolate* arg_isolate, byte* buffer,
                               unsigned buffer_size,
                               CodeObjectRequired create_code_object)
    : Assembler(arg_isolate, buffer, buffer_size),
      generating_stub_(false),
      has_frame_(false) {
  if (create_code_object == CodeObjectRequired::kYes) {
    code_object_ =
        Handle<Object>::New(isolate()->heap()->undefined_value(), isolate());
  }
}


void MacroAssembler::sethi(const Operand& src, Register d) {
//  Instruction* save_pc;
  int shiftcnt;
  bool relocatable = src.must_output_reloc_info(this);

  DCHECK(!relocatable);
 /* if (relocatable) {
    RecordRelocInfo(src.rmode_, src.immediate());
  }*/

#ifdef _LP64
# ifdef CHECK_DELAY
  assert_not_delayed();//(char*) "cannot put two instructions in delay slot");
# endif
//  v9_dep();
//  save_pc = pc();

  int msb32 = (int) (src.immediate() >> 32);
  int lsb32 = (int) (src.immediate());

  if (msb32 == 0 && lsb32 >= 0) {
    Assembler::sethi(lsb32, d);
  }
  else if (msb32 == -1) {
    Assembler::sethi(~lsb32, d);
    xor3(d, ~low10(~0), d);
  }
  else {
    Assembler::sethi(msb32, d);  // msb 22-bits
    if (msb32 & 0x3ff)                            // Any bits?
      or3(d, msb32 & 0x3ff, d);                   // msb 32-bits are now in lsb 32
    if (lsb32 & 0xFFFFFC00) {                     // done?
      if ((lsb32 >> 20) & 0xfff) {                // Any bits set?
        sllx(d, 12, d);                           // Make room for next 12 bits
        or3(d, (lsb32 >> 20) & 0xfff, d);         // Or in next 12
        shiftcnt = 0;                             // We already shifted
      }
      else
        shiftcnt = 12;
      if ((lsb32 >> 10) & 0x3ff) {
        sllx(d, shiftcnt + 10, d);                // Make room for last 10 bits
        or3(d, (lsb32 >> 10) & 0x3ff, d);         // Or in next 10
        shiftcnt = 0;
      }
      else
        shiftcnt = 10;
      sllx(d, shiftcnt + 10, d);                  // Shift leaving disp field 0'd
    }
    else
      sllx(d, 32, d);
  }
/*  // Pad out the instruction sequence so it can be patched later.
  if (relocatable) {
    while (pc() < (save_pc + (7 * kInstructionSize)))
      nop();
  }*/
#else
  Assembler::sethi(src.immediate(), d);
#endif
}
/*
int MacroAssembler::insts_for_sethi(int64_t a, bool worst_case) {
#ifdef _LP64
  if (worst_case)  return 7;
  intptr_t iaddr = (intptr_t) a;
  int msb32 = (int) (iaddr >> 32);
  int lsb32 = (int) (iaddr);
  int count;
  if (msb32 == 0 && lsb32 >= 0)
    count = 1;
  else if (msb32 == -1)
    count = 2;
  else {
    count = 2;
    if (msb32 & 0x3ff)
      count++;
    if (lsb32 & 0xFFFFFC00 ) {
      if ((lsb32 >> 20) & 0xfff)  count += 2;
      if ((lsb32 >> 10) & 0x3ff)  count += 2;
    }
  }
  return count;
#else
  return 1;
#endif
}

int MacroAssembler::worst_case_insts_for_set() {
  return insts_for_sethi(0, true) + 1;
}
*/
void MacroAssembler::set(const Operand& src, Register d) {
  intptr_t value = src.immediate();
  bool relocatable = src.must_output_reloc_info(this);

  if (relocatable) {
    RecordRelocInfo(src.rmode_);
# ifdef CHECK_DELAY
    assert_not_delayed();//(char*) "cannot put two instructions in delay slot");
# endif
 #ifdef _LP64
    int msb32 = static_cast<int>(value >> 32);
    int lsb32 = static_cast<int>(value);
    Assembler::sethi(msb32, d);  // msb 22-bits
    or3(d, msb32 & 0x3ff, d);                   // msb 32-bits are now in lsb 32
    sllx(d, 12, d);                           // Make room for next 12 bits
    or3(d, (lsb32 >> 20) & 0xfff, d);         // Or in next 12
    sllx(d, 10, d);                // Make room for last 10 bits
    or3(d, (lsb32 >> 10) & 0x3ff, d);         // Or in next 10
    sllx(d, 10, d);                  // Shift leaving disp field 0'd
 #else
    Assembler::sethi(value, d);
#endif
    add(d, low10(value), d);
    return;
  }

   // can optimize
  if (-4096 <= value && value <= 4095) {
    or3(g0, value, d); // setsw (this leaves upper 32 bits sign-extended)
    return;
  }
  if (inv_hi22(hi22(value)) == value) {
    sethi(src, d);
    return;
  }
  assert_not_delayed();//(char*) "cannot put two instructions in delay slot");
  sethi(src, d);
  int src_low10 = low10(src.immediate());
  if (src_low10 != 0) {
    add(d, src_low10, d);
  }
}


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
    


// Conditional breakpoint (for assertion checks in assembly code)
void MacroAssembler::breakpoint_trap(Condition c, CC cc) {
  trap(c, cc, g0, ST_BREAKPOINT);
}

// We want to use ST_BREAKPOINT here, but the debugger is confused by it.
void MacroAssembler::breakpoint_trap() {
  trap(ST_BREAKPOINT);
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

int MacroAssembler::CallSize(Address target, RelocInfo::Mode rmode) { 
    return (kInstructionsPerPachableSet + 2) * kInstructionSize;
 }

// MacroAssembler::CallSize is sensitive to changes in this function, as it
// requires to know how many instructions are used to branch to the target.
void MacroAssembler::Call(Address target, RelocInfo::Mode rmode) {
#ifdef DEBUG
  Label start_call;
  bind(&start_call);
#endif
  // Statement positions are expected to be recorded when the target
  // address is loaded.
  positions_recorder()->WriteRecordedPositions();
  set(Operand(reinterpret_cast<intptr_t>(target), rmode), kScratchRegister);
  callr(kScratchRegister, g0);
  delayed()->nop();
  
#ifdef DEBUG
  AssertSizeOfCodeGeneratedSince(&start_call, CallSize(target, rmode));
#endif
}
  
void MacroAssembler::Call(Handle<Code> code,
                          RelocInfo::Mode rmode,
                          TypeFeedbackId ast_id) {
/*#ifdef DEBUG
  Label start_call;
  bind(&start_call);
#endif
*/
  DCHECK(RelocInfo::IsCodeTarget(rmode));
  if ((rmode == RelocInfo::CODE_TARGET) && (!ast_id.IsNone())) {
    SetRecordedAstId(ast_id);
    rmode = RelocInfo::CODE_TARGET_WITH_ID;
  }

  AllowDeferredHandleDereference embedding_raw_address;
  Call(reinterpret_cast<Address>(code.location()), rmode);
/*
#ifdef DEBUG
  // Check the size of the code generated.
  AssertSizeOfCodeGeneratedSince(&start_call, CallSize(code, rmode, ast_id));
#endif*/
}


bool MacroAssembler::AllowThisStubCall(CodeStub* stub) {
  return has_frame_ || !stub->SometimesSetsUpAFrame();
}

void MacroAssembler::EnterFrame(StackFrame::Type type, bool load_constant_pool_pointer_reg) {
  // Out-of-line constant pool not implemented on sparc.
  UNREACHABLE();
}

void MacroAssembler::EnterFrame(StackFrame::Type type) {
  Save(2);
  set(Operand(Operand(CodeObject())), g1);
  st_ptr(g1, temp_address_in_frame(0));
  set(Operand(Smi::FromInt(type)), g1);
  st_ptr(g1, temp_address_in_frame(1));
} 

void MacroAssembler::LeaveFrame(StackFrame::Type type, int stack_adjustment) {
  restore();
}

void MacroAssembler::TailCallExternalReference(const ExternalReference& ext,
                                               int num_arguments,
                                               int result_size) {
     WARNING("MacroAssembler::TailCallExternalReference");
    breakpoint_trap();     
}


void MacroAssembler::CallStub(CodeStub* stub, TypeFeedbackId ast_id) {
  DCHECK(AllowThisStubCall(stub));  // Stub calls are not allowed in some stubs.
  Call(stub->GetCode(), RelocInfo::CODE_TARGET, ast_id);
}


void MacroAssembler::TailCallStub(CodeStub* stub) {
    UNIMPLEMENTED();
  //Jump(stub->GetCode(), RelocInfo::CODE_TARGET);
}

void MacroAssembler::CallRuntime(const Runtime::Function* f,
                                 int num_arguments,
                                 SaveFPRegsMode save_doubles) {
  // All arguments must be on the stack before this function is called.
  //o0 holds the return value after the call.

  // Check that the number of arguments matches what the function expects.
  // If f->nargs is -1, the function can accept a variable number of arguments.
  CHECK(f->nargs < 0 || f->nargs == num_arguments);

   // Place the necessary arguments.
  mov(num_arguments, o0);
  set(Operand(ExternalReference(f, isolate())), o1);
  CEntryStub stub(isolate(), 1, save_doubles);
  CallStub(&stub);
}


void MacroAssembler::TailCallRuntime(Runtime::FunctionId fid,
                                     int num_arguments,
                                     int result_size) {
  TailCallExternalReference(ExternalReference(fid, isolate()),
                            num_arguments,
                            result_size);
}


void MacroAssembler::LoadRoot(Heap::RootListIndex index, Register destination) {
  ld_ptr(MemOperand(kRootRegister, index << kPointerSizeLog2), destination);
}


void MacroAssembler::StoreRoot(Register source, Heap::RootListIndex index) {
  DCHECK(Heap::RootCanBeWrittenAfterInitialization(index));
  stx(source, MemOperand(kRootRegister, index << kPointerSizeLog2));
}

void MacroAssembler::Prologue(bool code_pre_aging, int locals_count) {
  PredictableCodeSizeScope predictible_code_size_scope(this, kNoCodeAgeSequenceLength);
  if (code_pre_aging) {
    UNIMPLEMENTED();
  } else {
      Save(locals_count);
  }
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



CodePatcher::CodePatcher(Isolate* isolate, byte* address, int instructions,
                         FlushICache flush_cache)
    : address_(address),
      size_(instructions * kInstructionSize),
      masm_(isolate, address, size_ + Assembler::kGap, CodeObjectRequired::kNo),
      flush_cache_(flush_cache) {
  // Create a new macro assembler pointing to the address of the code to patch.
  // The size is adjusted with kGap on order for the assembler to generate size
  // bytes of instructions without failing with buffer size constraints.
  DCHECK(masm_.reloc_info_writer.pos() == address_ + size_ + Assembler::kGap);
}


CodePatcher::~CodePatcher() {
  // Indicate that code has changed.
  if (flush_cache_ == FLUSH) {
    Assembler::FlushICache(masm_.isolate(), address_, size_);
  }
  // Check that the code was patched as expected.
  DCHECK(masm_.pc_ == address_ + size_);
  DCHECK(masm_.reloc_info_writer.pos() == address_ + size_ + Assembler::kGap);
}


//void CodePatcher::emit_int32(int instr) { masm()->emit_int32(instr); }


}  // namespace internal
}  // namespace v8

#endif  // V8_TARGET_ARCH_SPARC
