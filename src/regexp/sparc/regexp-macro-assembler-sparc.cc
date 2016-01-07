// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#if V8_TARGET_ARCH_SPARC

#include "src/regexp/sparc/regexp-macro-assembler-sparc.h"

#include "src/base/bits.h"
#include "src/code-stubs.h"
#include "src/log.h"
#include "src/macro-assembler.h"
#include "src/profiler/cpu-profiler.h"
#include "src/regexp/regexp-macro-assembler.h"
#include "src/regexp/regexp-stack.h"
#include "src/unicode.h"

namespace v8 {
namespace internal {

#ifndef V8_INTERPRETED_REGEXP
/*
 * This assembler uses the following register assignment convention
 * - r25: Temporarily stores the index of capture start after a matching pass
 *        for a global regexp.
 * - r26: Pointer to current code object (Code*) including heap object tag.
 * - r27: Current position in input, as negative offset from end of string.
 *        Please notice that this is the byte offset, not the character offset!
 * - r28: Currently loaded character. Must be loaded using
 *        LoadCurrentCharacter before using any of the dispatch methods.
 * - r29: Points to tip of backtrack stack
 * - r30: End of input (points to byte after last character in input).
 * - r31: Frame pointer. Used to access arguments, local variables and
 *         RegExp registers.
 * - r12: IP register, used by assembler. Very volatile.
 * - r1/sp : Points to tip of C stack.
 *
 * The remaining registers are free for computations.
 * Each call to a public method should retain this convention.
 *
 * The stack will have the following structure:
 *  - fp[44]  Isolate* isolate   (address of the current isolate)
 *  - fp[40]  secondary link/return address used by native call.
 *  - fp[36]  lr save area (currently unused)
 *  - fp[32]  backchain    (currently unused)
 *  --- sp when called ---
 *  - fp[28]  return address     (lr).
 *  - fp[24]  old frame pointer  (r31).
 *  - fp[0..20]  backup of registers r25..r30
 *  --- frame pointer ----
 *  - fp[-4]  direct_call        (if 1, direct call from JavaScript code,
 *                                if 0, call through the runtime system).
 *  - fp[-8]  stack_area_base    (high end of the memory area to use as
 *                                backtracking stack).
 *  - fp[-12] capture array size (may fit multiple sets of matches)
 *  - fp[-16] int* capture_array (int[num_saved_registers_], for output).
 *  - fp[-20] end of input       (address of end of string).
 *  - fp[-24] start of input     (address of first character in string).
 *  - fp[-28] start index        (character index of start).
 *  - fp[-32] void* input_string (location of a handle containing the string).
 *  - fp[-36] success counter    (only for global regexps to count matches).
 *  - fp[-40] Offset of location before start of input (effectively character
 *            string start - 1). Used to initialize capture registers to a
 *            non-position.
 *  - fp[-44] At start (if 1, we are starting at the start of the
 *    string, otherwise 0)
 *  - fp[-48] register 0         (Only positions must be stored in the first
 *  -         register 1          num_saved_registers_ registers)
 *  -         ...
 *  -         register num_registers-1
 *  --- sp ---
 *
 * The first num_saved_registers_ registers are initialized to point to
 * "character -1" in the string (i.e., char_size() bytes before the first
 * character of the string). The remaining registers start out as garbage.
 *
 * The data up to the return address must be placed there by the calling
 * code and the remaining arguments are passed in registers, e.g. by calling the
 * code entry as cast to a function with the signature:
 * int (*match)(String* input_string,
 *              int start_index,
 *              Address start,
 *              Address end,
 *              int* capture_output_array,
 *              byte* stack_area_base,
 *              Address secondary_return_address,  // Only used by native call.
 *              bool direct_call = false)
 * The call is performed by NativeRegExpMacroAssembler::Execute()
 * (in regexp-macro-assembler.cc) via the CALL_GENERATED_REGEXP_CODE macro
 * in sparc/simulator-sparc.h.
 * When calling as a non-direct call (i.e., from C++ code), the return address
 * area is overwritten with the LR register by the RegExp code. When doing a
 * direct call from generated code, the return address is placed there by
 * the calling code, as in a normal exit frame.
 */

#define __ ACCESS_MASM(masm_)

RegExpMacroAssemblerSPARC::RegExpMacroAssemblerSPARC(Isolate* isolate, Zone* zone,
                                                 Mode mode,
                                                 int registers_to_save)
    : NativeRegExpMacroAssembler(isolate, zone),
      masm_(new MacroAssembler(isolate, NULL, kRegExpCodeSize,
                               CodeObjectRequired::kYes)),
      mode_(mode),
      num_registers_(registers_to_save),
      num_saved_registers_(registers_to_save),
      entry_label_(),
      start_label_(),
      success_label_(),
      backtrack_label_(),
      exit_label_(),
      internal_failure_label_() {
  UNIMPLEMENTED();
}


RegExpMacroAssemblerSPARC::~RegExpMacroAssemblerSPARC() {
   UNIMPLEMENTED();
}


int RegExpMacroAssemblerSPARC::stack_limit_slack() {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::AdvanceCurrentPosition(int by) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::AdvanceRegister(int reg, int by) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::Backtrack() {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::Bind(Label* label) { __ bind(label); }


void RegExpMacroAssemblerSPARC::CheckCharacter(uint32_t c, Label* on_equal) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::CheckCharacterGT(uc16 limit, Label* on_greater) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::CheckAtStart(Label* on_at_start) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::CheckNotAtStart(int cp_offset,
                                              Label* on_not_at_start) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::CheckCharacterLT(uc16 limit, Label* on_less) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::CheckGreedyLoop(Label* on_equal) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::CheckNotBackReferenceIgnoreCase(
    int start_reg, bool read_backward, Label* on_no_match) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::CheckNotBackReference(int start_reg,
                                                    bool read_backward,
                                                    Label* on_no_match) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::CheckNotCharacter(unsigned c,
                                                Label* on_not_equal) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::CheckCharacterAfterAnd(uint32_t c, uint32_t mask,
                                                     Label* on_equal) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::CheckNotCharacterAfterAnd(unsigned c,
                                                        unsigned mask,
                                                        Label* on_not_equal) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::CheckNotCharacterAfterMinusAnd(
    uc16 c, uc16 minus, uc16 mask, Label* on_not_equal) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::CheckCharacterInRange(uc16 from, uc16 to,
                                                    Label* on_in_range) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::CheckCharacterNotInRange(uc16 from, uc16 to,
                                                       Label* on_not_in_range) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::CheckBitInTable(Handle<ByteArray> table,
                                              Label* on_bit_set) {
  UNIMPLEMENTED();
}


bool RegExpMacroAssemblerSPARC::CheckSpecialCharacterClass(uc16 type,
                                                         Label* on_no_match) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::Fail() {
  UNIMPLEMENTED();
}


Handle<HeapObject> RegExpMacroAssemblerSPARC::GetCode(Handle<String> source) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::GoTo(Label* to) { 
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::IfRegisterGE(int reg, int comparand,
                                           Label* if_ge) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::IfRegisterLT(int reg, int comparand,
                                           Label* if_lt) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::IfRegisterEqPos(int reg, Label* if_eq) {
  UNIMPLEMENTED();
}


RegExpMacroAssembler::IrregexpImplementation
RegExpMacroAssemblerSPARC::Implementation() {
  return kSPARCImplementation;
}


void RegExpMacroAssemblerSPARC::LoadCurrentCharacter(int cp_offset,
                                                   Label* on_end_of_input,
                                                   bool check_bounds,
                                                   int characters) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::PopCurrentPosition() {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::PopRegister(int register_index) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::PushBacktrack(Label* label) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::PushCurrentPosition() {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::PushRegister(int register_index,
                                           StackCheckFlag check_stack_limit) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::ReadCurrentPositionFromRegister(int reg) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::ReadStackPointerFromRegister(int reg) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::SetCurrentPositionFromEnd(int by) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::SetRegister(int register_index, int to) {
  UNIMPLEMENTED();
}


bool RegExpMacroAssemblerSPARC::Succeed() {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::WriteCurrentPositionToRegister(int reg,
                                                             int cp_offset) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::ClearRegisters(int reg_from, int reg_to) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::WriteStackPointerToRegister(int reg) {
  UNIMPLEMENTED();
}


// Private methods:

void RegExpMacroAssemblerSPARC::CallCheckStackGuardState(Register scratch) {
  UNIMPLEMENTED();
}


// Helper function for reading a value out of a stack frame.
template <typename T>
static T& frame_entry(Address re_frame, int frame_offset) {
  UNIMPLEMENTED();
}


template <typename T>
static T* frame_entry_address(Address re_frame, int frame_offset) {
  UNIMPLEMENTED();
}


int RegExpMacroAssemblerSPARC::CheckStackGuardState(Address* return_address,
                                                  Code* re_code,
                                                  Address re_frame) {
  UNIMPLEMENTED();
}


MemOperand RegExpMacroAssemblerSPARC::register_location(int register_index) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::CheckPosition(int cp_offset,
                                            Label* on_outside_input) {
  UNIMPLEMENTED();
}

/*
void RegExpMacroAssemblerSPARC::BranchOrBacktrack(Condition condition, Label* to,
                                                CRegister cr) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::SafeCall(Label* to, Condition cond,
                                       CRegister cr) {
  UNIMPLEMENTED();
}
*/

void RegExpMacroAssemblerSPARC::SafeReturn() {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::SafeCallTarget(Label* name) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::Push(Register source) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::Pop(Register target) {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::CheckPreemption() {
  UNIMPLEMENTED();
}


void RegExpMacroAssemblerSPARC::CheckStackLimit() {
  UNIMPLEMENTED();
}


bool RegExpMacroAssemblerSPARC::CanReadUnaligned() {
  UNIMPLEMENTED();
}

void RegExpMacroAssemblerSPARC::LoadCurrentCharacterUnchecked(int cp_offset,
                                                            int characters) {
  UNIMPLEMENTED();
}


#undef __

#endif  // V8_INTERPRETED_REGEXP
}  // namespace internal
}  // namespace v8

#endif  // V8_TARGET_ARCH_SPARC
