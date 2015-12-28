// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#if V8_TARGET_ARCH_SPARC

#include "src/codegen.h"
#include "src/debug/debug.h"
#include "src/deoptimizer.h"
#include "src/full-codegen/full-codegen.h"
#include "src/runtime/runtime.h"

namespace v8 {
namespace internal {


#define __ ACCESS_MASM(masm)


void Builtins::Generate_Adaptor(MacroAssembler* masm,
                                CFunctionId id,
                                BuiltinExtraArguments extra_args) {
    WARNING("Builtins::Generate_Adaptor");
    __ breakpoint_trap();
}

/*
// Load the built-in InternalArray function from the current context.
static void GenerateLoadInternalArrayFunction(MacroAssembler* masm,
                                              Register result) {
    WARNING("Builtins::");
}


// Load the built-in Array function from the current context.
static void GenerateLoadArrayFunction(MacroAssembler* masm, Register result) {
    WARNING("Builtins::");
}
*/

void Builtins::Generate_InternalArrayCode(MacroAssembler* masm) {
    WARNING("Builtins::Generate_InternalArrayCode");
    __ breakpoint_trap();
}


void Builtins::Generate_ArrayCode(MacroAssembler* masm) {
    WARNING("Builtins::Generate_ArrayCode");
    __ breakpoint_trap();
}


// static
void Builtins::Generate_StringConstructor(MacroAssembler* masm) {
    WARNING("Builtins::Generate_StringConstructor");
    __ breakpoint_trap();
}


void Builtins::Generate_StringConstructor_ConstructStub(MacroAssembler* masm) {
    WARNING("Builtins::Generate_StringConstructor_ConstructStub");
    __ breakpoint_trap();
}


/*static void CallRuntimePassFunction(
    MacroAssembler* masm, Runtime::FunctionId function_id) {
    WARNING("Builtins::");
}*/

/*
static void GenerateTailCallToSharedCode(MacroAssembler* masm) {
    WARNING("Builtins::");
}
*/

/*static void GenerateTailCallToReturnedCode(MacroAssembler* masm) {
    WARNING("Builtins::");
}*/


void Builtins::Generate_InOptimizationQueue(MacroAssembler* masm) {
    WARNING("Builtins::Generate_InOptimizationQueue");
    __ breakpoint_trap();
}


/*static void Generate_JSConstructStubHelper(MacroAssembler* masm,
                                           bool is_api_function) {
    WARNING("Builtins::");
}*/


void Builtins::Generate_JSConstructStubGeneric(MacroAssembler* masm) {
    WARNING("Builtins::Generate_JSConstructStubGeneric");
    __ breakpoint_trap();
}


void Builtins::Generate_JSBuiltinsConstructStub(MacroAssembler* masm) {
     WARNING("Builtins::Generate_JSBuiltinsConstructStub");
    __ breakpoint_trap();
}

void Builtins::Generate_ConstructedNonConstructable(MacroAssembler* masm) {
    WARNING("Builtins::Generate_JSConstructStubApi");
    __ breakpoint_trap();
}


void Builtins::Generate_JSConstructStubApi(MacroAssembler* masm) {
    WARNING("Builtins::Generate_JSConstructStubApi");
    __ breakpoint_trap();
}


enum IsTagged { kArgcIsSmiTagged, kArgcIsUntaggedInt };


// Clobbers r5; preserves all other registers.
static void Generate_CheckStackOverflow(MacroAssembler* masm, Register argc,
                                        IsTagged argc_is_tagged) {
/*  // Check the stack for overflow. We are not trying to catch
  // interruptions (e.g. debug break and preemption) here, so the "real stack
  // limit" is checked.
  Label okay;
  __ LoadRoot(r5, Heap::kRealStackLimitRootIndex);
  // Make r5 the space we have left. The stack might already be overflowed
  // here which will cause r5 to become negative.
  __ sub(r5, sp, r5);
  // Check if the arguments will overflow the stack.
  if (argc_is_tagged == kArgcIsSmiTagged) {
    __ SmiToPtrArrayOffset(r0, argc);
  } else {
    DCHECK(argc_is_tagged == kArgcIsUntaggedInt);
    __ ShiftLeftImm(r0, argc, Operand(kPointerSizeLog2));
  }
  __ cmp(r5, r0);
  __ bgt(&okay);  // Signed comparison.

  // Out of stack space.
  __ CallRuntime(Runtime::kThrowStackOverflow, 0);

  __ bind(&okay);*/
    WARNING("Generate_CheckStackOverflow");
  
}


// Clobbers a2; preserves all other registers.
/*static void Generate_CheckStackOverflow(MacroAssembler* masm, Register argc,
                                        IsTagged argc_is_tagged) {
    WARNING("Builtins::");
}*/

  // Called from Generate_JS_Entry
// Input:
//   o0: new.target.
//   o1: function.
//   o2: receiver.
//   o3: argc.
//   o4: argv.
// Output:
//   o0: result.
static void Generate_JSEntryTrampolineHelper(MacroAssembler* masm,
                                             bool is_construct) {
  Argument new_target(0, false);
  Argument function(1, false);
  Argument receiver(2, false);
  Argument argc(3, false);
  Argument argv(4, false);
  
  ProfileEntryHookStub::MaybeCallEntryHook(masm);

  // Clear the context before we push it when entering the internal frame.
  __ clr(cp);
  
    // Enter an internal frame.
  {
    __ RecordComment("Enter an internal frame");
    SparcFrameScope scope(masm, StackFrame::INTERNAL, argc.as_register());
    
    // Setup the context (we need to use the caller context from the isolate).
    ExternalReference context_address(Isolate::kContextAddress,
                                      masm->isolate());
     __ load_ptr_contents(Operand(context_address), cp);

    __ InitializeRootRegister();

    // Check if we have enough stack space to push all arguments.
    // Clobbers a2.
    Generate_CheckStackOverflow(masm, argc.as_in().as_register(), kArgcIsUntaggedInt);

    // Copy arguments to the stack in a loop.
  {
    __ RecordComment("Copy arguments to the stack in a loop");
    const Register dst = g3;
    const Register tmp = kScratchRegister;
    const Register offset = g2;
    int first_argument_offset = FrameConstants::memory_parameter_word_sp_offset * kWordSize + kStackBias;
    Label loop, exit;
    __ sll(argc.as_in().as_register(), kPointerSizeLog2, offset);
    __ cmp(offset, 0);
    __ br(Condition::lessEqual, false, Predict::pn, &exit);
    __ delayed()->add(sp, first_argument_offset, dst);
    __ ld_ptr(MemOperand(argv.as_in().as_register(), offset), tmp);
    __ bind(&loop);
    __ ld_ptr(MemOperand(tmp), tmp); // derreference parameter
    __ st_ptr(tmp, MemOperand(dst, offset));
    __ sub(offset, kPointerSize, offset);
    __ cmp(offset, 0);
    __ br(Condition::greater, true, Predict::pt, &loop);
    __ delayed()->ld_ptr(MemOperand(argv.as_in().as_register(), offset), tmp);
    __ bind(&exit);
  }

   
    // Push the function and the receiver onto the stack.
    __ mov(i1, o1); // copy function
    __ mov(i2, o2); // copy receiver.

    
    __ RecordComment("Initialize all JavaScript callee-saved registers");
    // Initialize all JavaScript callee-saved registers, since they will be seen
    // by the garbage collector as part of handlers.
    __ LoadRoot(Heap::kUndefinedValueRootIndex, kScratchRegister);
    __ mov(kScratchRegister, l0);
    __ mov(kScratchRegister, l1);
    __ mov(kScratchRegister, l2);
    __ mov(kScratchRegister, l3);
    __ mov(kScratchRegister, l4);
    __ mov(kScratchRegister, l5);
    __ mov(kScratchRegister, l6);
    __ mov(kScratchRegister, l7);
    
     // Invoke the code.
   __ RecordComment(" Invoke the code");
    Handle<Code> builtin = is_construct
                               ? masm->isolate()->builtins()->Construct()
                               : masm->isolate()->builtins()->Call();

 
    __ Call(builtin, RelocInfo::CODE_TARGET);
    // Leave internal frame.
  }
  __ ret();
  __ delayed()->nop();
}

void Builtins::Generate_JSEntryTrampoline(MacroAssembler* masm) {
  Generate_JSEntryTrampolineHelper(masm, false);
}


void Builtins::Generate_JSConstructEntryTrampoline(MacroAssembler* masm) {
  Generate_JSEntryTrampolineHelper(masm, true);
}


void Builtins::Generate_InterpreterEntryTrampoline(MacroAssembler* masm) {
    WARNING("Builtins::Generate_InterpreterEntryTrampoline");
    __ breakpoint_trap();
}


void Builtins::Generate_InterpreterExitTrampoline(MacroAssembler* masm) {
    WARNING("Builtins::Generate_InterpreterExitTrampoline");
    __ breakpoint_trap();
}


// static
void Builtins::Generate_InterpreterPushArgsAndCall(MacroAssembler* masm) {
    WARNING("Builtins::Generate_InterpreterPushArgsAndCall");
    __ breakpoint_trap();
}


// static
void Builtins::Generate_InterpreterPushArgsAndConstruct(MacroAssembler* masm) {
    WARNING("Builtins::Generate_InterpreterPushArgsAndConstruct");
    __ breakpoint_trap();
}


void Builtins::Generate_CompileLazy(MacroAssembler* masm) {
    WARNING("Builtins::Generate_CompileLazy");
    __ breakpoint_trap();
}


/*static void CallCompileOptimized(MacroAssembler* masm, bool concurrent) {
    WARNING("Builtins::");
}*/


void Builtins::Generate_CompileOptimized(MacroAssembler* masm) {
    WARNING("Builtins::Generate_CompileOptimized");
    __ breakpoint_trap();
}


void Builtins::Generate_CompileOptimizedConcurrent(MacroAssembler* masm) {
    WARNING("Builtins::Generate_CompileOptimizedConcurrent");
    __ breakpoint_trap();
}


static void GenerateMakeCodeYoungAgainCommon(MacroAssembler* masm) {
    WARNING("Builtins::GenerateMakeCodeYoungAgainCommon");
    __ breakpoint_trap();
}

#define DEFINE_CODE_AGE_BUILTIN_GENERATOR(C)                 \
void Builtins::Generate_Make##C##CodeYoungAgainEvenMarking(  \
    MacroAssembler* masm) {                                  \
  GenerateMakeCodeYoungAgainCommon(masm);                    \
}                                                            \
void Builtins::Generate_Make##C##CodeYoungAgainOddMarking(   \
    MacroAssembler* masm) {                                  \
  GenerateMakeCodeYoungAgainCommon(masm);                    \
}
CODE_AGE_LIST(DEFINE_CODE_AGE_BUILTIN_GENERATOR)
#undef DEFINE_CODE_AGE_BUILTIN_GENERATOR


void Builtins::Generate_MarkCodeAsExecutedOnce(MacroAssembler* masm) {
    WARNING("Builtins::Generate_MarkCodeAsExecutedOnce");
    __ breakpoint_trap();
}


void Builtins::Generate_MarkCodeAsExecutedTwice(MacroAssembler* masm) {
    WARNING("Builtins::Generate_MarkCodeAsExecutedTwice");
    __ breakpoint_trap();
}


void Builtins::Generate_MarkCodeAsToBeExecutedOnce(MacroAssembler* masm) {
    WARNING("Builtins::Generate_MarkCodeAsToBeExecutedOnce");
    __ breakpoint_trap();
}


/*static void Generate_NotifyStubFailureHelper(MacroAssembler* masm,
                                             SaveFPRegsMode save_doubles) {
    WARNING("Builtins::");
}*/


void Builtins::Generate_NotifyStubFailure(MacroAssembler* masm) {
    WARNING("Builtins::Generate_NotifyStubFailure");
    __ breakpoint_trap();
}


void Builtins::Generate_NotifyStubFailureSaveDoubles(MacroAssembler* masm) {
    WARNING("Builtins::Generate_NotifyStubFailureSaveDoubles");
    __ breakpoint_trap();
}


/*static void Generate_NotifyDeoptimizedHelper(MacroAssembler* masm,
                                             Deoptimizer::BailoutType type) {
    WARNING("Builtins::");
}*/


void Builtins::Generate_NotifyDeoptimized(MacroAssembler* masm) {
    WARNING("Builtins::Generate_NotifyDeoptimized");
    __ breakpoint_trap();
}


void Builtins::Generate_NotifySoftDeoptimized(MacroAssembler* masm) {
    WARNING("Builtins::Generate_NotifySoftDeoptimized");
    __ breakpoint_trap();
}


void Builtins::Generate_HandleFastApiCall(MacroAssembler* masm) {
        WARNING("Builtins::Generate_NotifySoftDeoptimized");
    __ breakpoint_trap();
}


void Builtins::Generate_NotifyLazyDeoptimized(MacroAssembler* masm) {
    WARNING("Builtins::Generate_NotifyLazyDeoptimized");
    __ breakpoint_trap();
}


void Builtins::Generate_OnStackReplacement(MacroAssembler* masm) {
    WARNING("Builtins::Generate_OnStackReplacement");
    __ breakpoint_trap();
}


void Builtins::Generate_OsrAfterStackCheck(MacroAssembler* masm) {
    WARNING("Builtins::Generate_OsrAfterStackCheck");
    __ breakpoint_trap();
}


// static
void Builtins::Generate_FunctionCall(MacroAssembler* masm) {
    WARNING("Builtins::Generate_FunctionCall");
    __ breakpoint_trap();
}


/*static void Generate_PushAppliedArguments(MacroAssembler* masm,
                                          const int vectorOffset,
                                          const int argumentsOffset,
                                          const int indexOffset,
                                          const int limitOffset) {
    WARNING("Builtins::");
}*/


// Used by FunctionApply and ReflectApply
/*static void Generate_ApplyHelper(MacroAssembler* masm, bool targetIsArgument) {
    WARNING("Builtins::");
}*/


static void Generate_ConstructHelper(MacroAssembler* masm) {
    WARNING("Generate_ConstructHelper");
    __ nop();
    __ nop();
    __ nop();
    __ nop();
    __ breakpoint_trap();
    __ nop();
    __ nop();
    __ nop();
    __ nop();
 }


void Builtins::Generate_FunctionApply(MacroAssembler* masm) {
    WARNING("Builtins::Generate_FunctionApply");
    __ breakpoint_trap();
}


void Builtins::Generate_ReflectApply(MacroAssembler* masm) {
    WARNING("Builtins::Generate_ReflectApply");
    __ breakpoint_trap();
}


void Builtins::Generate_ReflectConstruct(MacroAssembler* masm) {
    Generate_ConstructHelper(masm);
}


/*static void ArgumentAdaptorStackCheck(MacroAssembler* masm,
                                      Label* stack_overflow) {
    WARNING("Builtins::");
}*/


/*static void EnterArgumentsAdaptorFrame(MacroAssembler* masm) {
    WARNING("Builtins::");
}*/


/*static void LeaveArgumentsAdaptorFrame(MacroAssembler* masm) {
    WARNING("Builtins::");
}*/


// static
void Builtins::Generate_CallFunction(MacroAssembler* masm,
                                     ConvertReceiverMode mode) {
    WARNING("Builtins::Generate_CallFunction");
    __ nop();
}


// static
void Builtins::Generate_Call(MacroAssembler* masm, ConvertReceiverMode mode) {
    WARNING("Builtins::Generate_Call");
  // ----------- S t a t e -------------
  //  -- o0 : the number of arguments (not including the receiver)
  //  -- o1 : the function to call (checked to be a JSFunction)
  // -----------------------------------
  __ AssertFunction(o1);
}


void Builtins::Generate_ConstructFunction(MacroAssembler* masm) {
    WARNING("Builtins::Generate_ConstructFunction");
    __ breakpoint_trap();
}


// static
void Builtins::Generate_ConstructProxy(MacroAssembler* masm) {
    WARNING("Builtins::Generate_ConstructProxy");
    __ breakpoint_trap();
}


// static
void Builtins::Generate_Construct(MacroAssembler* masm) {
    WARNING("Builtins::Generate_Construct");
    __ breakpoint_trap();
}


void Builtins::Generate_ArgumentsAdaptorTrampoline(MacroAssembler* masm) {
    WARNING("Builtins::Generate_ArgumentsAdaptorTrampoline");
    __ breakpoint_trap();
}


#undef __

    
}  // namespace internal
}  // namespace v8

#endif  // V8_TARGET_ARCH_SPARC
