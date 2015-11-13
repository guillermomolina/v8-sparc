// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_SPARC_MACRO_ASSEMBLER_SPARC_H_
#define V8_SPARC_MACRO_ASSEMBLER_SPARC_H_

#include "src/assembler.h"
#include "src/bailout-reason.h"
#include "src/frames.h"
#include "src/globals.h"

namespace v8 {
namespace internal {

// Give alias names to registers for calling conventions.
const Register kReturnRegister0 = {Register::kCode_i0};
const Register kReturnRegister1 = {Register::kCode_i1};
const Register kJSFunctionRegister = {Register::kCode_g4};
const Register kContextRegister = {Register::kCode_g5};
const Register kInterpreterAccumulatorRegister = {Register::kCode_l0};
const Register kInterpreterRegisterFileRegister = {Register::kCode_l1};
const Register kInterpreterBytecodeOffsetRegister = {Register::kCode_l2};
const Register kInterpreterBytecodeArrayRegister = {Register::kCode_l3};
const Register kInterpreterDispatchTableRegister = {Register::kCode_l4};
const Register kJavaScriptCallArgCountRegister = {Register::kCode_i0};
const Register kRuntimeCallFunctionRegister = {Register::kCode_i1};
const Register kRuntimeCallArgCountRegister = {Register::kCode_i0};
   

// Flags used for AllocateHeapNumber
enum TaggingMode {
  // Tag the result.
  TAG_RESULT,
  // Don't tag
  DONT_TAG_RESULT
};

enum RememberedSetAction { EMIT_REMEMBERED_SET, OMIT_REMEMBERED_SET };
enum SmiCheck { INLINE_SMI_CHECK, OMIT_SMI_CHECK };
enum PointersToHereCheck {
  kPointersToHereMaybeInteresting,
  kPointersToHereAreAlwaysInteresting
};
enum LinkRegisterStatus { kLRHasNotBeenSaved, kLRHasBeenSaved };
enum RAStatus { kRAHasNotBeenSaved, kRAHasBeenSaved };

// MacroAssembler implements a collection of frequently used macros.
class MacroAssembler : public Assembler {
public:
  // The isolate parameter can be NULL if the macro assembler should
  // not use isolate-dependent functionality. In this case, it's the
  // responsibility of the caller to never invoke such function on the
  // macro assembler.
  MacroAssembler(Isolate* isolate, void* buffer, int size);
  
  
 // Returns the size of a call in instructions. Note, the value returned is
  // only valid as long as no entries are added to the constant pool between
  // checking the call size and emitting the actual call.
  static int CallSize(Register target)  { UNIMPLEMENTED(); }
  int CallSize(Address target, RelocInfo::Mode rmode, Condition condition = always) { UNIMPLEMENTED(); }
  static int CallSizeNotPredictableCodeSize(Address target,
                                            RelocInfo::Mode rmode,
                                            Condition condition = always) { UNIMPLEMENTED(); }

  // Jump, Call, and Ret pseudo instructions implementing inter-working.
  void Jump(Register target) { UNIMPLEMENTED(); }
  void JumpToJSEntry(Register target) { UNIMPLEMENTED(); }
  void Jump(Handle<Code> code, RelocInfo::Mode rmode, Condition condition = always) { UNIMPLEMENTED(); }
  void Call(Register target) { UNIMPLEMENTED(); }
  void CallJSEntry(Register target) { UNIMPLEMENTED(); }
  void Call(Address target, RelocInfo::Mode rmode, Condition condition = always) { UNIMPLEMENTED(); }
  int CallSize(Handle<Code> code,
               RelocInfo::Mode rmode = RelocInfo::CODE_TARGET,
               TypeFeedbackId ast_id = TypeFeedbackId::None(),
               Condition condition = always) { UNIMPLEMENTED(); }
  void Call(Handle<Code> code, RelocInfo::Mode rmode = RelocInfo::CODE_TARGET,
            TypeFeedbackId ast_id = TypeFeedbackId::None(),
            Condition condition = always) { UNIMPLEMENTED(); }
  void Ret()  { UNIMPLEMENTED(); }
 
  // Emit code to discard a non-negative number of pointer-sized elements
  // from the stack, clobbering only the sp register.
  void Drop(int count) { UNIMPLEMENTED(); }

  void Ret(int drop)  { UNIMPLEMENTED(); }

  void Call(Label* target) { UNIMPLEMENTED(); }

  // Emit call to the code we are currently generating.
  void CallSelf() {
    Handle<Code> self(reinterpret_cast<Code**>(CodeObject().location()));
    Call(self, RelocInfo::CODE_TARGET);
  }

  // Register move. May do nothing if the registers are identical.
  void Move(Register dst, Handle<Object> value) { UNIMPLEMENTED(); }
  void Move(Register dst, Register src, Condition condition = always) { UNIMPLEMENTED(); }
  void Move(DoubleRegister dst, DoubleRegister src) { UNIMPLEMENTED(); }

  // Load an object from the root table.
  void LoadRoot(Register destination, Heap::RootListIndex index,
                Condition condition = always) { UNIMPLEMENTED(); }
  // Store an object to the root table.
  void StoreRoot(Register source, Heap::RootListIndex index,
                 Condition condition = always) { UNIMPLEMENTED(); }

  void PushRoot(Heap::RootListIndex index) { UNIMPLEMENTED(); }

   void Push(Register src) { UNIMPLEMENTED(); }

  // Push a handle.
  void Push(Handle<Object> handle) { UNIMPLEMENTED(); }
  void Push(Smi* smi) { Push(Handle<Smi>(smi, isolate())); }

  void Pop(Register dst)  { UNIMPLEMENTED(); }
  
  // Verify restrictions about code generated in stubs.
  void set_generating_stub(bool value) { generating_stub_ = value; }
  bool generating_stub() { return generating_stub_; }
  void set_has_frame(bool value) { has_frame_ = value; }
  bool has_frame() { return has_frame_; }
  inline bool AllowThisStubCall(CodeStub* stub);

  // Activation support.
  void EnterFrame(StackFrame::Type type,
                  bool load_constant_pool_pointer_reg = false);
  // Returns the pc offset at which the frame ends.
  int LeaveFrame(StackFrame::Type type, int stack_adjustment = 0);
 
  // Tail call of a runtime routine (jump).
  // Like JumpToExternalReference, but also takes care of passing the number
  // of parameters.
  void TailCallExternalReference(const ExternalReference& ext,
                                 int num_arguments,
                                 int result_size);
 
  // Convenience function: tail call a runtime routine (jump).
  void TailCallRuntime(Runtime::FunctionId fid,
                       int num_arguments,
                       int result_size);


   // Allocates a heap number or jumps to the gc_required label if the young
  // space is full and a scavenge is needed. All registers are clobbered also
  // when control continues at the gc_required label.
  void AllocateHeapNumber(Register result,
                          Register scratch1,
                          Register scratch2,
                          Register heap_number_map,
                          Label* gc_required,
                          TaggingMode tagging_mode = TAG_RESULT,
                          MutableMode mode = IMMUTABLE) { UNIMPLEMENTED(); }

  
  // For a given |object| notify the garbage collector that the slot |address|
  // has been written.  |value| is the object being stored. The value and
  // address registers are clobbered by the operation.
  void RecordWrite(
      Register object,
      Register address,
      Register value,
      RAStatus ra_status,
      SaveFPRegsMode save_fp,
      RememberedSetAction remembered_set_action = EMIT_REMEMBERED_SET,
      SmiCheck smi_check = INLINE_SMI_CHECK,
      PointersToHereCheck pointers_to_here_check_for_value =
          kPointersToHereMaybeInteresting);

  // This is required for compatibility in architecture indepenedant code.
  inline void jmp(Label* L) { UNIMPLEMENTED(); }
  // -------------------------------------------------------------------------
  // Debugger Support.

  void DebugBreak() { UNIMPLEMENTED(); }

  // ---------------------------------------------------------------------------
  // Runtime calls

  // Call a code stub.
  void CallStub(CodeStub* stub, TypeFeedbackId ast_id = TypeFeedbackId::None(),
                Condition condition = always) { UNIMPLEMENTED(); }

  // Call a runtime routine.
  void CallRuntime(const Runtime::Function* f, int num_arguments,
                   SaveFPRegsMode save_doubles = kDontSaveFPRegs) { UNIMPLEMENTED(); }
  void CallRuntimeSaveDoubles(Runtime::FunctionId id)  { UNIMPLEMENTED(); }

  // Convenience function: Same as above, but takes the fid instead.
  void CallRuntime(Runtime::FunctionId id, int num_arguments,
                   SaveFPRegsMode save_doubles = kDontSaveFPRegs) { UNIMPLEMENTED(); }

  // Convenience function: call an external reference.
  void CallExternalReference(const ExternalReference& ext, int num_arguments) { UNIMPLEMENTED(); }


  // Call a code stub.
  void TailCallStub(CodeStub* stub, Condition condition = always) { UNIMPLEMENTED(); }

  Handle<Object> CodeObject() {
    DCHECK(!code_object_.is_null());
    return code_object_;
  }

  // Check if the map of an object is equal to a specified weak map and branch
  // to a specified target if equal. Skip the smi check if not required
  // (object is known to be a heap object)
  void DispatchWeakMap(Register obj, Register scratch1, Register scratch2,
                       Handle<WeakCell> cell, Handle<Code> success,
                       SmiCheckType smi_check_type) { UNIMPLEMENTED(); }

  // If the value is a NaN, canonicalize the value else, do nothing.
  void FPUCanonicalizeNaN(const DoubleRegister dst, const DoubleRegister src) { UNIMPLEMENTED(); }


  // Get value of the weak cell.
  void GetWeakValue(Register value, Handle<WeakCell> cell) { UNIMPLEMENTED(); }

  // Load the value of the weak cell in the value register. Branch to the
  // given miss label is the weak cell was cleared.
  void LoadWeakValue(Register value, Handle<WeakCell> cell, Label* miss) { UNIMPLEMENTED(); }

private:
  // Compute memory operands for safepoint stack slots.
  static int SafepointRegisterStackIndex(int reg_code) { UNIMPLEMENTED(); }
  MemOperand SafepointRegisterSlot(Register reg) { UNIMPLEMENTED(); }
  MemOperand SafepointRegistersAndDoublesSlot(Register reg) { UNIMPLEMENTED(); }

  bool generating_stub_;
  bool has_frame_;
  // This handle will be patched with the code object on installation.
  Handle<Object> code_object_;

  // Needs access to SafepointRegisterStackIndex for compiled frame
  // traversal.
  friend class StandardFrame;
};

#ifdef GENERATED_CODE_COVERAGE
#define CODE_COVERAGE_STRINGIFY(x) #x
#define CODE_COVERAGE_TOSTRING(x) CODE_COVERAGE_STRINGIFY(x)
#define __FILE_LINE__ __FILE__ ":" CODE_COVERAGE_TOSTRING(__LINE__)
#define ACCESS_MASM(masm) masm->stop(__FILE_LINE__); masm->
#else
#define ACCESS_MASM(masm) masm->
#endif

}  // namespace internal
}  // namespace v8

#endif  // V8_SPARC_MACRO_ASSEMBLER_SPARC_H_
