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
}


void Builtins::Generate_ArrayCode(MacroAssembler* masm) {
    WARNING("Builtins::Generate_ArrayCode");
}


// static
void Builtins::Generate_StringConstructor(MacroAssembler* masm) {
    WARNING("Builtins::Generate_StringConstructor");
}


void Builtins::Generate_StringConstructor_ConstructStub(MacroAssembler* masm) {
    WARNING("Builtins::Generate_StringConstructor_ConstructStub");
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
}


/*static void Generate_JSConstructStubHelper(MacroAssembler* masm,
                                           bool is_api_function) {
    WARNING("Builtins::");
}*/


void Builtins::Generate_JSConstructStubGeneric(MacroAssembler* masm) {
    WARNING("Builtins::Generate_JSConstructStubGeneric");
}


void Builtins::Generate_JSBuiltinsConstructStub(MacroAssembler* masm) {
     WARNING("Builtins::Generate_JSBuiltinsConstructStub");
}

void Builtins::Generate_JSConstructStubApi(MacroAssembler* masm) {
    WARNING("Builtins::Generate_JSConstructStubApi");
}


enum IsTagged { kArgcIsSmiTagged, kArgcIsUntaggedInt };


// Clobbers a2; preserves all other registers.
/*static void Generate_CheckStackOverflow(MacroAssembler* masm, Register argc,
                                        IsTagged argc_is_tagged) {
    WARNING("Builtins::");
}*/


/*static void Generate_JSEntryTrampolineHelper(MacroAssembler* masm,
                                             bool is_construct) {
    WARNING("Builtins::");
}*/


void Builtins::Generate_JSEntryTrampoline(MacroAssembler* masm) {
    WARNING("Builtins::Generate_JSEntryTrampoline");
}


void Builtins::Generate_JSConstructEntryTrampoline(MacroAssembler* masm) {
    WARNING("Builtins::Generate_JSConstructEntryTrampoline");
}


void Builtins::Generate_InterpreterEntryTrampoline(MacroAssembler* masm) {
    WARNING("Builtins::Generate_InterpreterEntryTrampoline");
}


void Builtins::Generate_InterpreterExitTrampoline(MacroAssembler* masm) {
    WARNING("Builtins::Generate_InterpreterExitTrampoline");
}


// static
void Builtins::Generate_InterpreterPushArgsAndCall(MacroAssembler* masm) {
    WARNING("Builtins::Generate_InterpreterPushArgsAndCall");
}


// static
void Builtins::Generate_InterpreterPushArgsAndConstruct(MacroAssembler* masm) {
    WARNING("Builtins::Generate_InterpreterPushArgsAndConstruct");
}


void Builtins::Generate_CompileLazy(MacroAssembler* masm) {
    WARNING("Builtins::Generate_CompileLazy");
}


/*static void CallCompileOptimized(MacroAssembler* masm, bool concurrent) {
    WARNING("Builtins::");
}*/


void Builtins::Generate_CompileOptimized(MacroAssembler* masm) {
    WARNING("Builtins::Generate_CompileOptimized");
}


void Builtins::Generate_CompileOptimizedConcurrent(MacroAssembler* masm) {
    WARNING("Builtins::Generate_CompileOptimizedConcurrent");
}


static void GenerateMakeCodeYoungAgainCommon(MacroAssembler* masm) {
    WARNING("Builtins::GenerateMakeCodeYoungAgainCommon");
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
}


void Builtins::Generate_MarkCodeAsExecutedTwice(MacroAssembler* masm) {
    WARNING("Builtins::Generate_MarkCodeAsExecutedTwice");
}


void Builtins::Generate_MarkCodeAsToBeExecutedOnce(MacroAssembler* masm) {
    WARNING("Builtins::Generate_MarkCodeAsToBeExecutedOnce");
}


/*static void Generate_NotifyStubFailureHelper(MacroAssembler* masm,
                                             SaveFPRegsMode save_doubles) {
    WARNING("Builtins::");
}*/


void Builtins::Generate_NotifyStubFailure(MacroAssembler* masm) {
    WARNING("Builtins::Generate_NotifyStubFailure");
}


void Builtins::Generate_NotifyStubFailureSaveDoubles(MacroAssembler* masm) {
    WARNING("Builtins::Generate_NotifyStubFailureSaveDoubles");
}


/*static void Generate_NotifyDeoptimizedHelper(MacroAssembler* masm,
                                             Deoptimizer::BailoutType type) {
    WARNING("Builtins::");
}*/


void Builtins::Generate_NotifyDeoptimized(MacroAssembler* masm) {
    WARNING("Builtins::Generate_NotifyDeoptimized");
}


void Builtins::Generate_NotifySoftDeoptimized(MacroAssembler* masm) {
    WARNING("Builtins::Generate_NotifySoftDeoptimized");
}


void Builtins::Generate_NotifyLazyDeoptimized(MacroAssembler* masm) {
    WARNING("Builtins::Generate_NotifyLazyDeoptimized");
}


void Builtins::Generate_OnStackReplacement(MacroAssembler* masm) {
    WARNING("Builtins::Generate_OnStackReplacement");
}


void Builtins::Generate_OsrAfterStackCheck(MacroAssembler* masm) {
    WARNING("Builtins::Generate_OsrAfterStackCheck");
}


// static
void Builtins::Generate_FunctionCall(MacroAssembler* masm) {
    WARNING("Builtins::Generate_FunctionCall");
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


/*static void Generate_ConstructHelper(MacroAssembler* masm) {
    WARNING("Builtins::");
}*/


void Builtins::Generate_FunctionApply(MacroAssembler* masm) {
    WARNING("Builtins::Generate_FunctionApply");
}


void Builtins::Generate_ReflectApply(MacroAssembler* masm) {
    WARNING("Builtins::Generate_ReflectApply");
}


void Builtins::Generate_ReflectConstruct(MacroAssembler* masm) {
    WARNING("Builtins::Generate_ReflectConstruct");
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
}


// static
void Builtins::Generate_Call(MacroAssembler* masm, ConvertReceiverMode mode) {
    WARNING("Builtins::Generate_Call");
}


void Builtins::Generate_ConstructFunction(MacroAssembler* masm) {
    WARNING("Builtins::Generate_ConstructFunction");
}


// static
void Builtins::Generate_ConstructProxy(MacroAssembler* masm) {
    WARNING("Builtins::Generate_ConstructProxy");
}


// static
void Builtins::Generate_Construct(MacroAssembler* masm) {
    WARNING("Builtins::Generate_Construct");
}


void Builtins::Generate_ArgumentsAdaptorTrampoline(MacroAssembler* masm) {
    WARNING("Builtins::Generate_ArgumentsAdaptorTrampoline");
}


#undef __

    
}  // namespace internal
}  // namespace v8

#endif  // V8_TARGET_ARCH_SPARC
