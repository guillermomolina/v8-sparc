// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#if V8_TARGET_ARCH_SPARC

#include "src/code-factory.h"
#include "src/code-stubs.h"
#include "src/codegen.h"
#include "src/debug/debug.h"
#include "src/full-codegen/full-codegen.h"
#include "src/ic/ic.h"
#include "src/parsing/parser.h"
#include "src/ast/scopes.h"

#include "src/sparc/code-stubs-sparc.h"
#include "src/sparc/macro-assembler-sparc.h"

namespace v8 {
namespace internal {

#define __ ACCESS_MASM(masm_)


// A patch site is a location in the code which it is possible to patch. This
// class has a number of methods to emit the code which is patchable and the
// method EmitPatchInfo to record a marker back to the patchable code. This
// marker is a andi zero_reg, rx, #yyyy instruction, and rx * 0x0000ffff + yyyy
// (raw 16 bit immediate value is used) is the delta from the pc to the first
// instruction of the patchable code.
// The marker instruction is effectively a NOP (dest is zero_reg) and will
// never be emitted by normal code.
class JumpPatchSite BASE_EMBEDDED {
 public:
  explicit JumpPatchSite(MacroAssembler* masm) : masm_(masm) {
#ifdef DEBUG
    info_emitted_ = false;
#endif
  }

  ~JumpPatchSite() {
    DCHECK(patch_site_.is_bound() == info_emitted_);
  }

  // When initially emitting this ensure that a jump is always generated to skip
  // the inlined smi code.
  void EmitJumpIfNotSmi(Register reg, Label* target) {
      WARNING("UNIMPLEMENTED");
  }

  // When initially emitting this ensure that a jump is never generated to skip
  // the inlined smi code.
  void EmitJumpIfSmi(Register reg, Label* target) {
      WARNING("UNIMPLEMENTED");
  }

  void EmitPatchInfo() {
      WARNING("UNIMPLEMENTED");
  }

 private:
  MacroAssembler* masm_;
  Label patch_site_;
#ifdef DEBUG
  bool info_emitted_;
#endif
};


// Generate code for a JS function.  On entry to the function the receiver
// and arguments have been pushed on the stack left to right.  The actual
// argument count matches the formal parameter count expected by the
// function.
//
// The live registers are:
//   o i0: the JS function object being called (i.e. ourselves)
//   o cp: our context (aka: g5)
//   o fp: our caller's frame pointer (aka: i6)
//   o sp: stack pointer (aka: o6)

//
// The function builds a JS frame.  Please see JavaScriptFrameConstants in
// sparc-mips.h for its layout.
void FullCodeGenerator::Generate() {
  CompilationInfo* info = info_;
  profiling_counter_ = isolate()->factory()->NewCell(
      Handle<Smi>(Smi::FromInt(FLAG_interrupt_budget), isolate()));
  SetFunctionPosition(literal());
  Comment cmnt(masm_, "[ function compiled by full code generator");

  ProfileEntryHookStub::MaybeCallEntryHook(masm_);

#ifdef DEBUG
  if (strlen(FLAG_stop_at) > 0 &&
      info->literal()->name()->IsUtf8EqualTo(CStrVector(FLAG_stop_at))) {
    WARNING("Untested");
    __ breakpoint_trap();
  }
#endif

  if (FLAG_debug_code && info->ExpectsJSReceiverAsReceiver()) {
    WARNING("UNIMPLEMENTED");
  }

  // Open a frame scope to indicate that there is a frame on the stack.  The
  // MANUAL indicates that the scope shouldn't actually generate code to set up
  // the frame (that is done below).
  FrameScope frame_scope(masm_, StackFrame::MANUAL);

  info->set_prologue_offset(masm_->pc_offset());

  { Comment cmnt(masm_, "[ Allocate locals");
    int locals_count = info->scope()->num_stack_slots();
    __ Prologue(info->IsCodePreAgingActive(), locals_count);
    // Generators allocate locals, if any, in context slots.
    DCHECK(!IsGeneratorFunction(info->literal()->kind()) || locals_count == 0);
    if (locals_count > 0) {
      if (locals_count >= 128) {
        WARNING("UNIMPLEMENTED");
      }
      WARNING("Untested");
      __ LoadRoot(Heap::kUndefinedValueRootIndex, l0);
      if(locals_count < 18) {
        for( int i = 1; i <= locals_count; i++) 
          __ stx(l1, MemOperand(fp, kStackBias - i * kPointerSize)); 
      } else {
        __ add(fp, kStackBias - kPointerSize, l0);
        __ add(fp, kStackBias - (locals_count + 1) * kPointerSize, l2);
        __ stx(l1, MemOperand(l0));
        Label loop;
        __ bind(&loop);
        __ add(l0, -kPointerSize, l0);
        __ cmp(l0, l2);
        __ brx(notEqual, true, pt, &loop);
        __ delayed()->stx(l1, MemOperand(l0));;	  
      }  
    }
  }
/* 
  bool function_in_register_i0 = true;

  // Possibly allocate a local context.
  if (info->scope()->num_heap_slots() > 0) {
    Comment cmnt(masm_, "[ Allocate context");
    bool need_write_barrier = true;
    int slots = info->scope()->num_heap_slots() - Context::MIN_CONTEXT_SLOTS;
    // Argument to NewContext is the function, which is still in i0.
    if (info->scope()->is_script_scope()) {
      WARNING("Untested");
      __ mov(i0, o0);
      __ mov(info->scope()->GetScopeInfo(info->isolate()), o1);
      __ CallRuntime(Runtime::kNewScriptContext, 2);
      PrepareForBailoutForId(BailoutId::ScriptContext(), TOS_REG);
    } else if (slots <= FastNewContextStub::kMaximumSlots) {
      WARNING("Untested");
      FastNewContextStub stub(isolate(), slots);
      __ CallStub(&stub);
      // Result of FastNewContextStub is always in new space.
      need_write_barrier = false;
    } else {
      WARNING("Untested");
      __ mov(i0, o0);
      __ CallRuntime(Runtime::kNewFunctionContext, 1);
    }
    function_in_register_i0 = false;
    // Context is returned in o0.  It replaces the context passed to us.
    // It's saved in the stack and kept live in cp.
      WARNING("Untested");
    __ mov(o0, cp);
    __ stx(o0, MemOperand(fp, StandardFrameConstants::kContextOffset));

    // Copy any necessary parameters into the context.
    int num_parameters = info->scope()->num_parameters();
    int first_parameter = info->scope()->has_this_declaration() ? -1 : 0;
    for (int i = first_parameter; i < num_parameters; i++) {
      Variable* var = (i == -1) ? scope()->receiver() : scope()->parameter(i);
      if (var->IsContextSlot()) {
        int parameter_offset = StandardFrameConstants::kCallerSPOffset +
                                 (num_parameters - 1 - i) * kPointerSize;
        // Load parameter from stack.
        __ ldx(MemOperand(fp, parameter_offset), l0);
        // Store it in the context.
        MemOperand target = ContextOperand(cp, var->index());
        __ stx(l0, target);

        // Update the write barrier.
        if (need_write_barrier) {
          __ RecordWriteContextSlot(
              cp, target.offset(), l0, l1, kDontSaveFPRegs);
        } else if (FLAG_debug_code) {
          Label done;
          __ JumpIfInNewSpace(cp, l0, &done);
          __ Abort(kExpectedNewSpaceObject);
          __ bind(&done);
        }
      }
    }
  }
 PrepareForBailoutForId(BailoutId::FunctionContext(), NO_REGISTERS);

  // Function register is trashed in case we bailout here. But since that
  // could happen only when we allocate a context the value of
  // |function_in_register_i0| is correct.

  // Possibly set up a local binding to the this function which is used in
  // derived constructors with super calls.
  Variable* this_function_var = scope()->this_function_var();
  if (this_function_var != nullptr) {
    Comment cmnt(masm_, "[ This function");
    if (!function_in_register_i0) {
      __ movp(rdi, Operand(rbp, JavaScriptFrameConstants::kFunctionOffset));
      // The write barrier clobbers register again, keep it marked as such.
    }
    SetVar(this_function_var, rdi, rbx, rdx);
  }

  Variable* new_target_var = scope()->new_target_var();
  if (new_target_var != nullptr) {
    Comment cmnt(masm_, "[ new.target");

    __ movp(rax, Operand(rbp, StandardFrameConstants::kCallerFPOffset));
    Label non_adaptor_frame;
    __ Cmp(Operand(rax, StandardFrameConstants::kContextOffset),
           Smi::FromInt(StackFrame::ARGUMENTS_ADAPTOR));
    __ j(not_equal, &non_adaptor_frame);
    __ movp(rax, Operand(rax, StandardFrameConstants::kCallerFPOffset));

    __ bind(&non_adaptor_frame);
    __ Cmp(Operand(rax, StandardFrameConstants::kMarkerOffset),
           Smi::FromInt(StackFrame::CONSTRUCT));

    Label non_construct_frame, done;
    __ j(not_equal, &non_construct_frame);

    // Construct frame
    __ movp(rax, Operand(rax, ConstructFrameConstants::kNewTargetOffset));
    __ jmp(&done);

    // Non-construct frame
    __ bind(&non_construct_frame);
    __ LoadRoot(rax, Heap::kUndefinedValueRootIndex);

    __ bind(&done);
    SetVar(new_target_var, rax, rbx, rdx);
  }

  // Possibly allocate an arguments object.
  Variable* arguments = scope()->arguments();
  if (arguments != NULL) {
    // Arguments object must be allocated after the context object, in
    // case the "arguments" or ".arguments" variables are in the context.
    Comment cmnt(masm_, "[ Allocate arguments object");
    DCHECK(rdi.is(ArgumentsAccessNewDescriptor::function()));
    if (!function_in_register_i0) {
      __ movp(rdi, Operand(rbp, JavaScriptFrameConstants::kFunctionOffset));
    }
    // The receiver is just before the parameters on the caller's stack.
    int num_parameters = info->scope()->num_parameters();
    int offset = num_parameters * kPointerSize;
    __ Move(ArgumentsAccessNewDescriptor::parameter_count(),
            Smi::FromInt(num_parameters));
    __ leap(ArgumentsAccessNewDescriptor::parameter_pointer(),
            Operand(rbp, StandardFrameConstants::kCallerSPOffset + offset));

    // Arguments to ArgumentsAccessStub:
    //   function, parameter pointer, parameter count.
    // The stub will rewrite parameter pointer and parameter count if the
    // previous stack frame was an arguments adapter frame.
    bool is_unmapped = is_strict(language_mode()) || !has_simple_parameters();
    ArgumentsAccessStub::Type type = ArgumentsAccessStub::ComputeType(
        is_unmapped, literal()->has_duplicate_parameters());
    ArgumentsAccessStub stub(isolate(), type);
    __ CallStub(&stub);

    SetVar(arguments, rax, rbx, rdx);
  }

  if (FLAG_trace) {
    __ CallRuntime(Runtime::kTraceEnter, 0);
  }

  // Visit the declarations and body unless there is an illegal
  // redeclaration.
  if (scope()->HasIllegalRedeclaration()) {
    Comment cmnt(masm_, "[ Declarations");
    VisitForEffect(scope()->GetIllegalRedeclaration());

  } else {
    PrepareForBailoutForId(BailoutId::FunctionEntry(), NO_REGISTERS);
    { Comment cmnt(masm_, "[ Declarations");
      VisitDeclarations(scope()->declarations());
    }

    // Assert that the declarations do not use ICs. Otherwise the debugger
    // won't be able to redirect a PC at an IC to the correct IC in newly
    // recompiled code.
    DCHECK_EQ(0, ic_total_count_);

    { Comment cmnt(masm_, "[ Stack check");
      PrepareForBailoutForId(BailoutId::Declarations(), NO_REGISTERS);
       Label ok;
       __ CompareRoot(rsp, Heap::kStackLimitRootIndex);
       __ j(above_equal, &ok, Label::kNear);
       __ call(isolate()->builtins()->StackCheck(), RelocInfo::CODE_TARGET);
       __ bind(&ok);
    }

    { Comment cmnt(masm_, "[ Body");
      DCHECK(loop_depth() == 0);
      VisitStatements(literal()->body());
      DCHECK(loop_depth() == 0);
    }
  }
*/
  // Always emit a 'return undefined' in case control fell off the end of
  // the body.
  { Comment cmnt(masm_, "[ return <undefined>;");
    __ LoadRoot(Heap::kUndefinedValueRootIndex, o0);
    EmitReturnSequence();
  }
}


void FullCodeGenerator::ClearAccumulator() {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitProfilingCounterDecrement(int delta) {
      WARNING("FullCodeGenerator::EmitProfilingCounterDecrement");
}


void FullCodeGenerator::EmitProfilingCounterReset() {
      WARNING("FullCodeGenerator::EmitProfilingCounterReset");
}


void FullCodeGenerator::EmitBackEdgeBookkeeping(IterationStatement* stmt,
                                                Label* back_edge_target) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitReturnSequence() {
  Comment cmnt(masm_, "[ Return sequence");
  if (return_label_.is_bound()) {
    __ jmp(&return_label_);
  } else {
    __ bind(&return_label_);
    if (FLAG_trace) {
      __ mov(i0, o0);
      __ CallRuntime(Runtime::kTraceExit, 1);
    }
    // Pretend that the exit is a backwards jump to the entry.
    int weight = 1;
    if (info_->ShouldSelfOptimize()) {
      weight = FLAG_interrupt_budget / FLAG_self_opt_count;
    } else {
      int distance = masm_->pc_offset();
      weight = Min(kMaxBackEdgeWeight,
                   Max(1, distance / kCodeSizeMultiplier));
    }
    EmitProfilingCounterDecrement(weight);
    Label ok;
	__ brx(notEqual, true, pt, &ok);
	__ delayed()->nop();
 //   __ j(positive, &ok, Label::kNear);
    __ mov(i0, o0);
    __ Call(isolate()->builtins()->InterruptCheck(),
            RelocInfo::CODE_TARGET);
    EmitProfilingCounterReset();
    __ bind(&ok);

    SetReturnPosition(literal());
    __ ret(); 
    __ delayed()->restore(); // free the stack
  }
}


void FullCodeGenerator::StackValueContext::Plug(Variable* var) const {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EffectContext::Plug(Heap::RootListIndex index) const {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::AccumulatorValueContext::Plug(
    Heap::RootListIndex index) const {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::StackValueContext::Plug(
    Heap::RootListIndex index) const {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::TestContext::Plug(Heap::RootListIndex index) const {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EffectContext::Plug(Handle<Object> lit) const {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::AccumulatorValueContext::Plug(
    Handle<Object> lit) const {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::StackValueContext::Plug(Handle<Object> lit) const {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::TestContext::Plug(Handle<Object> lit) const {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EffectContext::DropAndPlug(int count,
                                                   Register reg) const {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::AccumulatorValueContext::DropAndPlug(
    int count,
    Register reg) const {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::StackValueContext::DropAndPlug(int count,
                                                       Register reg) const {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::TestContext::DropAndPlug(int count,
                                                 Register reg) const {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EffectContext::Plug(Label* materialize_true,
                                            Label* materialize_false) const {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::AccumulatorValueContext::Plug(
    Label* materialize_true,
    Label* materialize_false) const {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::StackValueContext::Plug(
    Label* materialize_true,
    Label* materialize_false) const {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::TestContext::Plug(Label* materialize_true,
                                          Label* materialize_false) const {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::AccumulatorValueContext::Plug(bool flag) const {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::StackValueContext::Plug(bool flag) const {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::TestContext::Plug(bool flag) const {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::DoTest(Expression* condition,
                               Label* if_true,
                               Label* if_false,
                               Label* fall_through) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::Split(Condition cc,
                              Label* if_true,
                              Label* if_false,
                              Label* fall_through) {
      WARNING("UNIMPLEMENTED");
}


MemOperand FullCodeGenerator::StackOperand(Variable* var) {
      WARNING("UNIMPLEMENTED");
      return MemOperand(sp);
}


MemOperand FullCodeGenerator::VarOperand(Variable* var, Register scratch) {
      WARNING("UNIMPLEMENTED");
      return MemOperand(fp);
}


void FullCodeGenerator::GetVar(Register dest, Variable* var) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::SetVar(Variable* var,
                               Register src,
                               Register scratch0,
                               Register scratch1) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::PrepareForBailoutBeforeSplit(Expression* expr,
                                                     bool should_normalize,
                                                     Label* if_true,
                                                     Label* if_false) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitDebugCheckDeclarationContext(Variable* variable) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::VisitVariableDeclaration(
    VariableDeclaration* declaration) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::VisitFunctionDeclaration(
    FunctionDeclaration* declaration) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::DeclareGlobals(Handle<FixedArray> pairs) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::DeclareModules(Handle<FixedArray> descriptions) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::VisitSwitchStatement(SwitchStatement* stmt) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::VisitForInStatement(ForInStatement* stmt) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitNewClosure(Handle<SharedFunctionInfo> info,
                                       bool pretenure) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitSetHomeObject(Expression* initializer, int offset,
                                          FeedbackVectorSlot slot) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitSetHomeObjectAccumulator(Expression* initializer,
                                                     int offset,
                                                     FeedbackVectorSlot slot) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitLoadGlobalCheckExtensions(VariableProxy* proxy,
                                                      TypeofMode typeof_mode,
                                                      Label* slow) {
      WARNING("UNIMPLEMENTED");
}


MemOperand FullCodeGenerator::ContextSlotOperandCheckExtensions(Variable* var,
                                                                Label* slow) {
      WARNING("UNIMPLEMENTED");
      return MemOperand(cp);
}


void FullCodeGenerator::EmitDynamicLookupFastCase(VariableProxy* proxy,
                                                  TypeofMode typeof_mode,
                                                  Label* slow, Label* done) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitGlobalVariableLoad(VariableProxy* proxy,
                                               TypeofMode typeof_mode) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitVariableLoad(VariableProxy* proxy,
                                         TypeofMode typeof_mode) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::VisitRegExpLiteral(RegExpLiteral* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitAccessor(ObjectLiteralProperty* property) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::VisitObjectLiteral(ObjectLiteral* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::VisitArrayLiteral(ArrayLiteral* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::VisitAssignment(Assignment* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::VisitYield(Yield* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitGeneratorResume(Expression *generator,
    Expression *value,
    JSGeneratorObject::ResumeMode resume_mode) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitCreateIteratorResult(bool done) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitNamedPropertyLoad(Property* prop) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitNamedSuperPropertyLoad(Property* prop) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitKeyedPropertyLoad(Property* prop) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitKeyedSuperPropertyLoad(Property* prop) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitInlineSmiBinaryOp(BinaryOperation* expr,
                                              Token::Value op,
                                              Expression* left_expr,
                                              Expression* right_expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitClassDefineProperties(ClassLiteral* lit) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitBinaryOp(BinaryOperation* expr, Token::Value op) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitAssignment(Expression* expr,
                                       FeedbackVectorSlot slot) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitStoreToStackLocalOrContextSlot(
    Variable* var, MemOperand location) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitVariableAssignment(Variable* var, Token::Value op,
                                               FeedbackVectorSlot slot) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitNamedPropertyAssignment(Assignment* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitNamedSuperPropertyStore(Property* prop) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitKeyedSuperPropertyStore(Property* prop) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitKeyedPropertyAssignment(Assignment* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::VisitProperty(Property* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::CallIC(Handle<Code> code,
                               TypeFeedbackId id) {
      WARNING("UNIMPLEMENTED");
}


// Code common for calls using the IC.
void FullCodeGenerator::EmitCallWithLoadIC(Call* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitSuperCallWithLoadIC(Call* expr) {
      WARNING("UNIMPLEMENTED");
}


// Code common for calls using the IC.
void FullCodeGenerator::EmitKeyedCallWithLoadIC(Call* expr,
                                                Expression* key) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitKeyedSuperCallWithLoadIC(Call* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitCall(Call* expr, ConvertReceiverMode mode) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitResolvePossiblyDirectEval(int arg_count) {
      WARNING("UNIMPLEMENTED");
}


// See http://www.ecma-international.org/ecma-262/6.0/#sec-function-calls.
void FullCodeGenerator::PushCalleeAndWithBaseObject(Call* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitPossiblyEvalCall(Call* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::VisitCallNew(CallNew* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitSuperConstructorCall(Call* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitIsSmi(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitIsSimdValue(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitIsFunction(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitIsMinusZero(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitIsArray(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitIsTypedArray(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitIsRegExp(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitIsJSProxy(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitIsConstructCall(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitObjectEquals(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitArguments(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitArgumentsLength(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitClassOf(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitValueOf(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitIsDate(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitDateField(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitOneByteSeqStringSetChar(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitTwoByteSeqStringSetChar(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitSetValueOf(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitToInteger(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitToName(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitStringCharFromCode(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitStringCharCodeAt(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitStringCharAt(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitCall(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitDefaultConstructorCallSuper(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitHasCachedArrayIndex(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitGetCachedArrayIndex(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitFastOneByteArrayJoin(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitDebugIsActive(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitCreateIterResultObject(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitLoadJSRuntimeFunction(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitCallJSRuntimeFunction(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::VisitCallRuntime(CallRuntime* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::VisitUnaryOperation(UnaryOperation* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::VisitCountOperation(CountOperation* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitLiteralCompareTypeof(Expression* expr,
                                                 Expression* sub_expr,
                                                 Handle<String> check) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::VisitCompareOperation(CompareOperation* expr) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitLiteralCompareNil(CompareOperation* expr,
                                              Expression* sub_expr,
                                              NilValue nil) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::VisitThisFunction(ThisFunction* expr) {
      WARNING("UNIMPLEMENTED");
}


Register FullCodeGenerator::result_register() {
      WARNING("UNIMPLEMENTED");
      return no_reg;
}


Register FullCodeGenerator::context_register() {
      WARNING("UNIMPLEMENTED");
      return no_reg;
}


void FullCodeGenerator::StoreToFrameField(int frame_offset, Register value) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::LoadContextField(Register dst, int context_index) {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::PushFunctionArgumentForContextAllocation() {
      WARNING("UNIMPLEMENTED");
}


// ----------------------------------------------------------------------------
// Non-local control flow support.

void FullCodeGenerator::EnterFinallyBlock() {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::ExitFinallyBlock() {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::ClearPendingMessage() {
      WARNING("UNIMPLEMENTED");
}


void FullCodeGenerator::EmitLoadStoreICSlot(FeedbackVectorSlot slot) {
      WARNING("UNIMPLEMENTED");
}


#undef __


void BackEdgeTable::PatchAt(Code* unoptimized_code,
                            Address pc,
                            BackEdgeState target_state,
                            Code* replacement_code) {
      WARNING("UNIMPLEMENTED");
}


BackEdgeTable::BackEdgeState BackEdgeTable::GetBackEdgeState(
    Isolate* isolate,
    Code* unoptimized_code,
    Address pc) {
      WARNING("UNIMPLEMENTED");
      return BackEdgeTable::BackEdgeState();
}

}  // namespace internal
}  // namespace v8
#endif  // V8_TARGET_ARCH_SPARC
