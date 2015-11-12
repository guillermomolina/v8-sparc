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
    UNIMPLEMENTED();
}

/*
// Load the built-in InternalArray function from the current context.
static void GenerateLoadInternalArrayFunction(MacroAssembler* masm,
                                              Register result) {
    UNIMPLEMENTED();
}


// Load the built-in Array function from the current context.
static void GenerateLoadArrayFunction(MacroAssembler* masm, Register result) {
    UNIMPLEMENTED();
}
*/

void Builtins::Generate_InternalArrayCode(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void Builtins::Generate_ArrayCode(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


// static
void Builtins::Generate_StringConstructor(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void Builtins::Generate_StringConstructor_ConstructStub(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


/*static void CallRuntimePassFunction(
    MacroAssembler* masm, Runtime::FunctionId function_id) {
    UNIMPLEMENTED();
}*/

/*
static void GenerateTailCallToSharedCode(MacroAssembler* masm) {
    UNIMPLEMENTED();
}
*/

/*static void GenerateTailCallToReturnedCode(MacroAssembler* masm) {
    UNIMPLEMENTED();
}*/


void Builtins::Generate_InOptimizationQueue(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


/*static void Generate_JSConstructStubHelper(MacroAssembler* masm,
                                           bool is_api_function) {
    UNIMPLEMENTED();
}*/


void Builtins::Generate_JSConstructStubGeneric(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void Builtins::Generate_JSConstructStubApi(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void Builtins::Generate_JSConstructStubForDerived(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


enum IsTagged { kArgcIsSmiTagged, kArgcIsUntaggedInt };


// Clobbers a2; preserves all other registers.
/*static void Generate_CheckStackOverflow(MacroAssembler* masm, Register argc,
                                        IsTagged argc_is_tagged) {
    UNIMPLEMENTED();
}*/


/*static void Generate_JSEntryTrampolineHelper(MacroAssembler* masm,
                                             bool is_construct) {
    UNIMPLEMENTED();
}*/


void Builtins::Generate_JSEntryTrampoline(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void Builtins::Generate_JSConstructEntryTrampoline(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void Builtins::Generate_InterpreterEntryTrampoline(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void Builtins::Generate_InterpreterExitTrampoline(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


// static
void Builtins::Generate_InterpreterPushArgsAndCall(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


// static
void Builtins::Generate_InterpreterPushArgsAndConstruct(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void Builtins::Generate_CompileLazy(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


/*static void CallCompileOptimized(MacroAssembler* masm, bool concurrent) {
    UNIMPLEMENTED();
}*/


void Builtins::Generate_CompileOptimized(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void Builtins::Generate_CompileOptimizedConcurrent(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


static void GenerateMakeCodeYoungAgainCommon(MacroAssembler* masm) {
    UNIMPLEMENTED();
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
    UNIMPLEMENTED();
}


void Builtins::Generate_MarkCodeAsExecutedTwice(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void Builtins::Generate_MarkCodeAsToBeExecutedOnce(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


/*static void Generate_NotifyStubFailureHelper(MacroAssembler* masm,
                                             SaveFPRegsMode save_doubles) {
    UNIMPLEMENTED();
}*/


void Builtins::Generate_NotifyStubFailure(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void Builtins::Generate_NotifyStubFailureSaveDoubles(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


/*static void Generate_NotifyDeoptimizedHelper(MacroAssembler* masm,
                                             Deoptimizer::BailoutType type) {
    UNIMPLEMENTED();
}*/


void Builtins::Generate_NotifyDeoptimized(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void Builtins::Generate_NotifySoftDeoptimized(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void Builtins::Generate_NotifyLazyDeoptimized(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void Builtins::Generate_OnStackReplacement(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void Builtins::Generate_OsrAfterStackCheck(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


// static
void Builtins::Generate_FunctionCall(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


/*static void Generate_PushAppliedArguments(MacroAssembler* masm,
                                          const int vectorOffset,
                                          const int argumentsOffset,
                                          const int indexOffset,
                                          const int limitOffset) {
    UNIMPLEMENTED();
}*/


// Used by FunctionApply and ReflectApply
/*static void Generate_ApplyHelper(MacroAssembler* masm, bool targetIsArgument) {
    UNIMPLEMENTED();
}*/


/*static void Generate_ConstructHelper(MacroAssembler* masm) {
    UNIMPLEMENTED();
}*/


void Builtins::Generate_FunctionApply(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void Builtins::Generate_ReflectApply(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void Builtins::Generate_ReflectConstruct(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


/*static void ArgumentAdaptorStackCheck(MacroAssembler* masm,
                                      Label* stack_overflow) {
    UNIMPLEMENTED();
}*/


/*static void EnterArgumentsAdaptorFrame(MacroAssembler* masm) {
    UNIMPLEMENTED();
}*/


/*static void LeaveArgumentsAdaptorFrame(MacroAssembler* masm) {
    UNIMPLEMENTED();
}*/


// static
void Builtins::Generate_CallFunction(MacroAssembler* masm,
                                     ConvertReceiverMode mode) {
    UNIMPLEMENTED();
}


// static
void Builtins::Generate_Call(MacroAssembler* masm, ConvertReceiverMode mode) {
    UNIMPLEMENTED();
}


void Builtins::Generate_ConstructFunction(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


// static
void Builtins::Generate_ConstructProxy(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


// static
void Builtins::Generate_Construct(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void Builtins::Generate_ArgumentsAdaptorTrampoline(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


#undef __

    
}  // namespace internal
}  // namespace v8

#endif  // V8_TARGET_ARCH_SPARC
