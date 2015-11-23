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
#include "src/parser.h"
#include "src/scopes.h"

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
      UNIMPLEMENTED();
  }

  // When initially emitting this ensure that a jump is never generated to skip
  // the inlined smi code.
  void EmitJumpIfSmi(Register reg, Label* target) {
      UNIMPLEMENTED();
  }

  void EmitPatchInfo() {
      UNIMPLEMENTED();
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
//   o a1: the JS function object being called (i.e. ourselves)
//   o cp: our context
//   o fp: our caller's frame pointer
//   o sp: stack pointer

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
    UNIMPLEMENTED();
  }
#endif

  if (FLAG_debug_code && info->ExpectsJSReceiverAsReceiver()) {
    UNIMPLEMENTED();
  }

  // Open a frame scope to indicate that there is a frame on the stack.  The
  // MANUAL indicates that the scope shouldn't actually generate code to set up
  // the frame (that is done below).
  FrameScope frame_scope(masm_, StackFrame::MANUAL);

  info->set_prologue_offset(masm_->pc_offset());
  __ Prologue(info->IsCodePreAgingActive());

  { Comment cmnt(masm_, "[ Allocate locals");
    int locals_count = info->scope()->num_stack_slots();
    // Generators allocate locals, if any, in context slots.
    DCHECK(!IsGeneratorFunction(info->literal()->kind()) || locals_count == 0);
    if (locals_count > 0) {
      if (locals_count >= 128) {
		UNIMPLEMENTED();
      }
      __ LoadRoot(g2, Heap::kUndefinedValueRootIndex);
	  
	  
	 /* 
	  kMaxPushes = 18 en SPARC
      if (locals_count >= kMaxPushes) {
        int loop_iterations = locals_count / kMaxPushes;
        __ mov(loop_iterations, l1);
        Label loop_header;
        __ bind(&loop_header);
        // Do pushes.
        __ sub(sp, Operand(kMaxPushes * kPointerSize), sp);
        for (int i = 0; i < kMaxPushes; i++) {
          __ stdx(l0, MemOperand(sp, i * kPointerSize));
        }
        // Continue loop if not done.
        __ Dsubu(l1, l1, Operand(1));
        __ Branch(&loop_header, ne, l1, Operand(zero_reg));
      }
      int remaining = locals_count % kMaxPushes;
      // Emit the remaining pushes.
      __ Dsubu(sp, sp, Operand(remaining * kPointerSize));
      for (int i  = 0; i < remaining; i++) {
        __ sd(l0, MemOperand(sp, i * kPointerSize));
      }*/
    }
  }
/*
  bool function_in_register = true;

  // Possibly allocate a local context.
  if (info->scope()->num_heap_slots() > 0) {
    Comment cmnt(masm_, "[ Allocate context");
    bool need_write_barrier = true;
    int slots = info->scope()->num_heap_slots() - Context::MIN_CONTEXT_SLOTS;
    // Argument to NewContext is the function, which is still in rdi.
    if (info->scope()->is_script_scope()) {
      __ Push(rdi);
      __ Push(info->scope()->GetScopeInfo(info->isolate()));
      __ CallRuntime(Runtime::kNewScriptContext, 2);
      PrepareForBailoutForId(BailoutId::ScriptContext(), TOS_REG);
    } else if (slots <= FastNewContextStub::kMaximumSlots) {
      FastNewContextStub stub(isolate(), slots);
      __ CallStub(&stub);
      // Result of FastNewContextStub is always in new space.
      need_write_barrier = false;
    } else {
      __ Push(rdi);
      __ CallRuntime(Runtime::kNewFunctionContext, 1);
    }
    function_in_register = false;
    // Context is returned in rax.  It replaces the context passed to us.
    // It's saved in the stack and kept live in rsi.
    __ movp(rsi, rax);
    __ movp(Operand(rbp, StandardFrameConstants::kContextOffset), rax);

    // Copy any necessary parameters into the context.
    int num_parameters = info->scope()->num_parameters();
    int first_parameter = info->scope()->has_this_declaration() ? -1 : 0;
    for (int i = first_parameter; i < num_parameters; i++) {
      Variable* var = (i == -1) ? scope()->receiver() : scope()->parameter(i);
      if (var->IsContextSlot()) {
        int parameter_offset = StandardFrameConstants::kCallerSPOffset +
            (num_parameters - 1 - i) * kPointerSize;
        // Load parameter from stack.
        __ movp(rax, Operand(rbp, parameter_offset));
        // Store it in the context.
        int context_offset = Context::SlotOffset(var->index());
        __ movp(Operand(rsi, context_offset), rax);
        // Update the write barrier.  This clobbers rax and rbx.
        if (need_write_barrier) {
          __ RecordWriteContextSlot(
              rsi, context_offset, rax, rbx, kDontSaveFPRegs);
        } else if (FLAG_debug_code) {
          Label done;
          __ JumpIfInNewSpace(rsi, rax, &done, Label::kNear);
          __ Abort(kExpectedNewSpaceObject);
          __ bind(&done);
        }
      }
    }
  }
  PrepareForBailoutForId(BailoutId::FunctionContext(), NO_REGISTERS);

  // Function register is trashed in case we bailout here. But since that
  // could happen only when we allocate a context the value of
  // |function_in_register| is correct.

  // Possibly set up a local binding to the this function which is used in
  // derived constructors with super calls.
  Variable* this_function_var = scope()->this_function_var();
  if (this_function_var != nullptr) {
    Comment cmnt(masm_, "[ This function");
    if (!function_in_register) {
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
    if (!function_in_register) {
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

  // Always emit a 'return undefined' in case control fell off the end of
  // the body.
  { Comment cmnt(masm_, "[ return <undefined>;");
    __ LoadRoot(rax, Heap::kUndefinedValueRootIndex);
    EmitReturnSequence();
  }*/
}


void FullCodeGenerator::ClearAccumulator() {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitProfilingCounterDecrement(int delta) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitProfilingCounterReset() {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitBackEdgeBookkeeping(IterationStatement* stmt,
                                                Label* back_edge_target) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitReturnSequence() {
      UNIMPLEMENTED();
}


void FullCodeGenerator::StackValueContext::Plug(Variable* var) const {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EffectContext::Plug(Heap::RootListIndex index) const {
      UNIMPLEMENTED();
}


void FullCodeGenerator::AccumulatorValueContext::Plug(
    Heap::RootListIndex index) const {
      UNIMPLEMENTED();
}


void FullCodeGenerator::StackValueContext::Plug(
    Heap::RootListIndex index) const {
      UNIMPLEMENTED();
}


void FullCodeGenerator::TestContext::Plug(Heap::RootListIndex index) const {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EffectContext::Plug(Handle<Object> lit) const {
      UNIMPLEMENTED();
}


void FullCodeGenerator::AccumulatorValueContext::Plug(
    Handle<Object> lit) const {
      UNIMPLEMENTED();
}


void FullCodeGenerator::StackValueContext::Plug(Handle<Object> lit) const {
      UNIMPLEMENTED();
}


void FullCodeGenerator::TestContext::Plug(Handle<Object> lit) const {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EffectContext::DropAndPlug(int count,
                                                   Register reg) const {
      UNIMPLEMENTED();
}


void FullCodeGenerator::AccumulatorValueContext::DropAndPlug(
    int count,
    Register reg) const {
      UNIMPLEMENTED();
}


void FullCodeGenerator::StackValueContext::DropAndPlug(int count,
                                                       Register reg) const {
      UNIMPLEMENTED();
}


void FullCodeGenerator::TestContext::DropAndPlug(int count,
                                                 Register reg) const {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EffectContext::Plug(Label* materialize_true,
                                            Label* materialize_false) const {
      UNIMPLEMENTED();
}


void FullCodeGenerator::AccumulatorValueContext::Plug(
    Label* materialize_true,
    Label* materialize_false) const {
      UNIMPLEMENTED();
}


void FullCodeGenerator::StackValueContext::Plug(
    Label* materialize_true,
    Label* materialize_false) const {
      UNIMPLEMENTED();
}


void FullCodeGenerator::TestContext::Plug(Label* materialize_true,
                                          Label* materialize_false) const {
      UNIMPLEMENTED();
}


void FullCodeGenerator::AccumulatorValueContext::Plug(bool flag) const {
      UNIMPLEMENTED();
}


void FullCodeGenerator::StackValueContext::Plug(bool flag) const {
      UNIMPLEMENTED();
}


void FullCodeGenerator::TestContext::Plug(bool flag) const {
      UNIMPLEMENTED();
}


void FullCodeGenerator::DoTest(Expression* condition,
                               Label* if_true,
                               Label* if_false,
                               Label* fall_through) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::Split(Condition cc,
                              Label* if_true,
                              Label* if_false,
                              Label* fall_through) {
      UNIMPLEMENTED();
}


MemOperand FullCodeGenerator::StackOperand(Variable* var) {
      UNIMPLEMENTED();
}


MemOperand FullCodeGenerator::VarOperand(Variable* var, Register scratch) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::GetVar(Register dest, Variable* var) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::SetVar(Variable* var,
                               Register src,
                               Register scratch0,
                               Register scratch1) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::PrepareForBailoutBeforeSplit(Expression* expr,
                                                     bool should_normalize,
                                                     Label* if_true,
                                                     Label* if_false) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitDebugCheckDeclarationContext(Variable* variable) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::VisitVariableDeclaration(
    VariableDeclaration* declaration) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::VisitFunctionDeclaration(
    FunctionDeclaration* declaration) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::DeclareGlobals(Handle<FixedArray> pairs) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::DeclareModules(Handle<FixedArray> descriptions) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::VisitSwitchStatement(SwitchStatement* stmt) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::VisitForInStatement(ForInStatement* stmt) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitNewClosure(Handle<SharedFunctionInfo> info,
                                       bool pretenure) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitSetHomeObject(Expression* initializer, int offset,
                                          FeedbackVectorSlot slot) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitSetHomeObjectAccumulator(Expression* initializer,
                                                     int offset,
                                                     FeedbackVectorSlot slot) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitLoadGlobalCheckExtensions(VariableProxy* proxy,
                                                      TypeofMode typeof_mode,
                                                      Label* slow) {
      UNIMPLEMENTED();
}


MemOperand FullCodeGenerator::ContextSlotOperandCheckExtensions(Variable* var,
                                                                Label* slow) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitDynamicLookupFastCase(VariableProxy* proxy,
                                                  TypeofMode typeof_mode,
                                                  Label* slow, Label* done) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitGlobalVariableLoad(VariableProxy* proxy,
                                               TypeofMode typeof_mode) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitVariableLoad(VariableProxy* proxy,
                                         TypeofMode typeof_mode) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::VisitRegExpLiteral(RegExpLiteral* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitAccessor(ObjectLiteralProperty* property) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::VisitObjectLiteral(ObjectLiteral* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::VisitArrayLiteral(ArrayLiteral* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::VisitAssignment(Assignment* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::VisitYield(Yield* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitGeneratorResume(Expression *generator,
    Expression *value,
    JSGeneratorObject::ResumeMode resume_mode) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitCreateIteratorResult(bool done) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitNamedPropertyLoad(Property* prop) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitNamedSuperPropertyLoad(Property* prop) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitKeyedPropertyLoad(Property* prop) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitKeyedSuperPropertyLoad(Property* prop) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitInlineSmiBinaryOp(BinaryOperation* expr,
                                              Token::Value op,
                                              Expression* left_expr,
                                              Expression* right_expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitClassDefineProperties(ClassLiteral* lit) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitBinaryOp(BinaryOperation* expr, Token::Value op) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitAssignment(Expression* expr,
                                       FeedbackVectorSlot slot) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitStoreToStackLocalOrContextSlot(
    Variable* var, MemOperand location) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitVariableAssignment(Variable* var, Token::Value op,
                                               FeedbackVectorSlot slot) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitNamedPropertyAssignment(Assignment* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitNamedSuperPropertyStore(Property* prop) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitKeyedSuperPropertyStore(Property* prop) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitKeyedPropertyAssignment(Assignment* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::VisitProperty(Property* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::CallIC(Handle<Code> code,
                               TypeFeedbackId id) {
      UNIMPLEMENTED();
}


// Code common for calls using the IC.
void FullCodeGenerator::EmitCallWithLoadIC(Call* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitSuperCallWithLoadIC(Call* expr) {
      UNIMPLEMENTED();
}


// Code common for calls using the IC.
void FullCodeGenerator::EmitKeyedCallWithLoadIC(Call* expr,
                                                Expression* key) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitKeyedSuperCallWithLoadIC(Call* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitCall(Call* expr, ConvertReceiverMode mode) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitResolvePossiblyDirectEval(int arg_count) {
      UNIMPLEMENTED();
}


// See http://www.ecma-international.org/ecma-262/6.0/#sec-function-calls.
void FullCodeGenerator::PushCalleeAndWithBaseObject(Call* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitPossiblyEvalCall(Call* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::VisitCallNew(CallNew* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitSuperConstructorCall(Call* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitIsSmi(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitIsSpecObject(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitIsSimdValue(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitIsFunction(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitIsMinusZero(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitIsArray(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitIsTypedArray(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitIsRegExp(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitIsJSProxy(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitIsConstructCall(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitObjectEquals(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitArguments(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitArgumentsLength(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitClassOf(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitValueOf(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitIsDate(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitDateField(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitOneByteSeqStringSetChar(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitTwoByteSeqStringSetChar(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitSetValueOf(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitToInteger(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitToName(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitStringCharFromCode(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitStringCharCodeAt(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitStringCharAt(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitCall(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitDefaultConstructorCallSuper(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitHasCachedArrayIndex(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitGetCachedArrayIndex(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitFastOneByteArrayJoin(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitDebugIsActive(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitCreateIterResultObject(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitLoadJSRuntimeFunction(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitCallJSRuntimeFunction(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::VisitCallRuntime(CallRuntime* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::VisitUnaryOperation(UnaryOperation* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::VisitCountOperation(CountOperation* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitLiteralCompareTypeof(Expression* expr,
                                                 Expression* sub_expr,
                                                 Handle<String> check) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::VisitCompareOperation(CompareOperation* expr) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitLiteralCompareNil(CompareOperation* expr,
                                              Expression* sub_expr,
                                              NilValue nil) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::VisitThisFunction(ThisFunction* expr) {
      UNIMPLEMENTED();
}


Register FullCodeGenerator::result_register() {
      UNIMPLEMENTED();
}


Register FullCodeGenerator::context_register() {
      UNIMPLEMENTED();
}


void FullCodeGenerator::StoreToFrameField(int frame_offset, Register value) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::LoadContextField(Register dst, int context_index) {
      UNIMPLEMENTED();
}


void FullCodeGenerator::PushFunctionArgumentForContextAllocation() {
      UNIMPLEMENTED();
}


// ----------------------------------------------------------------------------
// Non-local control flow support.

void FullCodeGenerator::EnterFinallyBlock() {
      UNIMPLEMENTED();
}


void FullCodeGenerator::ExitFinallyBlock() {
      UNIMPLEMENTED();
}


void FullCodeGenerator::ClearPendingMessage() {
      UNIMPLEMENTED();
}


void FullCodeGenerator::EmitLoadStoreICSlot(FeedbackVectorSlot slot) {
      UNIMPLEMENTED();
}


#undef __


void BackEdgeTable::PatchAt(Code* unoptimized_code,
                            Address pc,
                            BackEdgeState target_state,
                            Code* replacement_code) {
      UNIMPLEMENTED();
}


BackEdgeTable::BackEdgeState BackEdgeTable::GetBackEdgeState(
    Isolate* isolate,
    Code* unoptimized_code,
    Address pc) {
      UNIMPLEMENTED();
}

}  // namespace internal
}  // namespace v8
#endif  // V8_TARGET_ARCH_SPARC
