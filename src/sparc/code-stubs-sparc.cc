// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#if V8_TARGET_ARCH_SPARC

#include "src/base/bits.h"
#include "src/bootstrapper.h"
#include "src/code-stubs.h"
#include "src/codegen.h"
#include "src/ic/handler-compiler.h"
#include "src/ic/ic.h"
#include "src/ic/stub-cache.h"
#include "src/isolate.h"
#include "src/sparc/code-stubs-sparc.h"
#include "src/regexp/jsregexp.h"
#include "src/regexp/regexp-macro-assembler.h"
#include "src/runtime/runtime.h"

namespace v8 {
namespace internal {

/*
static void InitializeArrayConstructorDescriptor(
    Isolate* isolate, CodeStubDescriptor* descriptor,
    int constant_stack_parameter_count) {
    UNIMPLEMENTED();
}


static void InitializeInternalArrayConstructorDescriptor(
    Isolate* isolate, CodeStubDescriptor* descriptor,
    int constant_stack_parameter_count) {
    UNIMPLEMENTED();
}
*/

void ArrayNoArgumentConstructorStub::InitializeDescriptor(
    CodeStubDescriptor* descriptor) {
    UNIMPLEMENTED();
}


void ArraySingleArgumentConstructorStub::InitializeDescriptor(
    CodeStubDescriptor* descriptor) {
    UNIMPLEMENTED();
}


void ArrayNArgumentsConstructorStub::InitializeDescriptor(
    CodeStubDescriptor* descriptor) {
    UNIMPLEMENTED();
}


void InternalArrayNoArgumentConstructorStub::InitializeDescriptor(
    CodeStubDescriptor* descriptor) {
    UNIMPLEMENTED();
}


void InternalArraySingleArgumentConstructorStub::InitializeDescriptor(
    CodeStubDescriptor* descriptor) {
    UNIMPLEMENTED();
}


void InternalArrayNArgumentsConstructorStub::InitializeDescriptor(
    CodeStubDescriptor* descriptor) {
    UNIMPLEMENTED();
}


#define __ ACCESS_MASM(masm)

/*
static void EmitIdenticalObjectComparison(MacroAssembler* masm, Label* slow,
                                          Condition cc, Strength strength);
static void EmitSmiNonsmiComparison(MacroAssembler* masm,
                                    Register lhs,
                                    Register rhs,
                                    Label* rhs_not_nan,
                                    Label* slow,
                                    bool strict);
static void EmitStrictTwoHeapObjectCompare(MacroAssembler* masm,
                                           Register lhs,
                                           Register rhs);
*/

void HydrogenCodeStub::GenerateLightweightMiss(MacroAssembler* masm,
                                               ExternalReference miss) {
    UNIMPLEMENTED();
}


void DoubleToIStub::Generate(MacroAssembler* masm) {
    UNIMPLEMENTED();
}

/*
// Handle the case where the lhs and rhs are the same object.
// Equality is almost reflexive (everything but NaN), so this is a test
// for "identity and not NaN".
static void EmitIdenticalObjectComparison(MacroAssembler* masm, Label* slow,
                                          Condition cc, Strength strength) {
    UNIMPLEMENTED();
}


static void EmitSmiNonsmiComparison(MacroAssembler* masm,
                                    Register lhs,
                                    Register rhs,
                                    Label* both_loaded_as_doubles,
                                    Label* slow,
                                    bool strict) {
    UNIMPLEMENTED();
}


static void EmitStrictTwoHeapObjectCompare(MacroAssembler* masm,
                                           Register lhs,
                                           Register rhs) {
    UNIMPLEMENTED();
}


static void EmitCheckForTwoHeapNumbers(MacroAssembler* masm,
                                       Register lhs,
                                       Register rhs,
                                       Label* both_loaded_as_doubles,
                                       Label* not_heap_numbers,
                                       Label* slow) {
    UNIMPLEMENTED();
}


// Fast negative check for internalized-to-internalized equality.
static void EmitCheckForInternalizedStringsOrObjects(MacroAssembler* masm,
                                                     Register lhs,
                                                     Register rhs,
                                                     Label* possible_strings,
                                                     Label* not_both_strings) {
    UNIMPLEMENTED();
}


static void CompareICStub_CheckInputType(MacroAssembler* masm, Register input,
                                         Register scratch,
                                         CompareICState::State expected,
                                         Label* fail) {
    UNIMPLEMENTED();
}
*/

// On entry a1 and a2 are the values to be compared.
// On exit a0 is 0, positive or negative to indicate the result of
// the comparison.
void CompareICStub::GenerateGeneric(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void StoreRegistersStateStub::Generate(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void RestoreRegistersStateStub::Generate(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void StoreBufferOverflowStub::Generate(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void MathPowStub::Generate(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


bool CEntryStub::NeedsImmovableCode() {
    UNIMPLEMENTED();
}


void CodeStub::GenerateStubsAheadOfTime(Isolate* isolate) {
    WARNING("CodeStub::GenerateStubsAheadOfTime");
}


void StoreRegistersStateStub::GenerateAheadOfTime(Isolate* isolate) {
    UNIMPLEMENTED();
}


void RestoreRegistersStateStub::GenerateAheadOfTime(Isolate* isolate) {
    UNIMPLEMENTED();
}


void CodeStub::GenerateFPStubs(Isolate* isolate) {
    WARNING("CodeStub::GenerateFPStubs");
}


void CEntryStub::GenerateAheadOfTime(Isolate* isolate) {
    UNIMPLEMENTED();
}





void CEntryStub::Generate(MacroAssembler* masm) {
    WARNING("CEntryStub::Generate");
  __ retl(); 
  __ delayed()->nop(); 
}


// This is the entry point from C++. 5 arguments are provided in o0-o4.
// See use of the CALL_GENERATED_CODE macro for example in src/execution.cc.
// Input:
//   o0: code entry.
//   o1: function.
//   o2: receiver.
//   o3: argc.
//   o4: argv.
// Output:
//   o0: result.
void JSEntryStub::Generate(MacroAssembler* masm) {
    WARNING("JSEntryStub::Generate");
  // Clear any pending exceptions.
  __ set(Operand(isolate()->factory()->the_hole_value()),g1);
  __ set(Operand(ExternalReference(Isolate::kPendingExceptionAddress, isolate())), g2);
  __ stx(g1, MemOperand(g2));

  __ retl(); 
  __ delayed()->mov(o2, o0); 
}


void LoadIndexedStringStub::Generate(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void InstanceOfStub::Generate(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void FunctionPrototypeStub::Generate(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void ArgumentsAccessStub::GenerateReadElement(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void ArgumentsAccessStub::GenerateNewSloppySlow(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void ArgumentsAccessStub::GenerateNewSloppyFast(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void LoadIndexedInterceptorStub::Generate(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void ArgumentsAccessStub::GenerateNewStrict(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void RegExpExecStub::Generate(MacroAssembler* masm) {
    UNIMPLEMENTED();
}

/*
static void CallStubInRecordCallTarget(MacroAssembler* masm, CodeStub* stub,
                                       bool is_super) {
    UNIMPLEMENTED();
}


static void GenerateRecordCallTarget(MacroAssembler* masm, bool is_super) { UNIMPLEMENTED(); }

*/
void CallConstructStub::Generate(MacroAssembler* masm) {
    UNIMPLEMENTED(); 
}


// StringCharCodeAtGenerator.
void StringCharCodeAtGenerator::GenerateFast(MacroAssembler* masm) {
    UNIMPLEMENTED(); 
}


void CallICStub::HandleArrayCase(MacroAssembler* masm, Label* miss) { UNIMPLEMENTED(); }


void CallICStub::Generate(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void CallICStub::GenerateMiss(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void StringCharCodeAtGenerator::GenerateSlow(
    MacroAssembler* masm, EmbedMode embed_mode,
    const RuntimeCallHelper& call_helper) {
   UNIMPLEMENTED(); 
}


// -------------------------------------------------------------------------
// StringCharFromCodeGenerator

void StringCharFromCodeGenerator::GenerateFast(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void StringCharFromCodeGenerator::GenerateSlow(
    MacroAssembler* masm, const RuntimeCallHelper& call_helper) {
   UNIMPLEMENTED(); 
}


enum CopyCharactersFlags { COPY_ONE_BYTE = 1, DEST_ALWAYS_ALIGNED = 2 };


void StringHelper::GenerateCopyCharacters(MacroAssembler* masm,
                                          Register dest,
                                          Register src,
                                          Register count,
                                          Register scratch,
                                          String::Encoding encoding) {
   UNIMPLEMENTED(); 
}


void SubStringStub::Generate(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void ToNumberStub::Generate(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void ToLengthStub::Generate(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void ToStringStub::Generate(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void StringHelper::GenerateFlatOneByteStringEquals(
    MacroAssembler* masm, Register left, Register right, Register scratch1,
    Register scratch2, Register scratch3) {
   UNIMPLEMENTED(); 
}


void StringHelper::GenerateCompareFlatOneByteStrings(
    MacroAssembler* masm, Register left, Register right, Register scratch1,
    Register scratch2, Register scratch3, Register scratch4) {
   UNIMPLEMENTED(); 
}


void StringHelper::GenerateOneByteCharsCompareLoop(
    MacroAssembler* masm, Register left, Register right, Register length,
    Register scratch1, Register scratch2, Register scratch3,
    Label* chars_not_equal) {
   UNIMPLEMENTED(); 
}


void StringCompareStub::Generate(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void BinaryOpICWithAllocationSiteStub::Generate(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void CompareICStub::GenerateBooleans(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void CompareICStub::GenerateSmis(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void CompareICStub::GenerateNumbers(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void CompareICStub::GenerateInternalizedStrings(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void CompareICStub::GenerateUniqueNames(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void CompareICStub::GenerateStrings(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void CompareICStub::GenerateObjects(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void CompareICStub::GenerateKnownObjects(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void CompareICStub::GenerateMiss(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void DirectCEntryStub::Generate(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void DirectCEntryStub::GenerateCall(MacroAssembler* masm,
                                    Register target) {
   UNIMPLEMENTED(); 
}


void NameDictionaryLookupStub::GenerateNegativeLookup(MacroAssembler* masm,
                                                      Label* miss,
                                                      Label* done,
                                                      Register receiver,
                                                      Register properties,
                                                      Handle<Name> name,
                                                      Register scratch0) {
   UNIMPLEMENTED(); 
}


// Probe the name dictionary in the |elements| register. Jump to the
// |done| label if a property with the given name is found. Jump to
// the |miss| label otherwise.
// If lookup was successful |scratch2| will be equal to elements + 4 * index.
void NameDictionaryLookupStub::GeneratePositiveLookup(MacroAssembler* masm,
                                                      Label* miss,
                                                      Label* done,
                                                      Register elements,
                                                      Register name,
                                                      Register scratch1,
                                                      Register scratch2) {
   UNIMPLEMENTED(); 
}


void NameDictionaryLookupStub::Generate(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void StoreBufferOverflowStub::GenerateFixedRegStubsAheadOfTime(
    Isolate* isolate) {
    WARNING("StoreBufferOverflowStub::GenerateFixedRegStubsAheadOfTime");
}


// Takes the input in 3 registers: address_ value_ and object_.  A pointer to
// the value has just been written into the object, now this stub makes sure
// we keep the GC informed.  The word in the object where the value has been
// written is in the address register.
void RecordWriteStub::Generate(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void RecordWriteStub::GenerateIncremental(MacroAssembler* masm, Mode mode) {
   UNIMPLEMENTED(); 
}


void RecordWriteStub::InformIncrementalMarker(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void RecordWriteStub::CheckNeedsToInformIncrementalMarker(
    MacroAssembler* masm,
    OnNoNeedToInformIncrementalMarker on_no_need,
    Mode mode) {
   UNIMPLEMENTED(); 
}


void StubFailureTrampolineStub::Generate(MacroAssembler* masm) {
    WARNING("StubFailureTrampolineStub::Generate");
}


void LoadICTrampolineStub::Generate(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void KeyedLoadICTrampolineStub::Generate(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void CallICTrampolineStub::Generate(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void LoadICStub::Generate(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void LoadICStub::GenerateForTrampoline(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}

/*
static void HandleArrayCases(MacroAssembler* masm, Register feedback,
                             Register receiver_map, Register scratch1,
                             Register scratch2, bool is_polymorphic,
                             Label* miss) {
   UNIMPLEMENTED(); 
}


static void HandleMonomorphicCase(MacroAssembler* masm, Register receiver,
                                  Register receiver_map, Register feedback,
                                  Register vector, Register slot,
                                  Register scratch, Label* compare_map,
                                  Label* load_smi_map, Label* try_array) {
   UNIMPLEMENTED(); 
}
*/

void LoadICStub::GenerateImpl(MacroAssembler* masm, bool in_frame) {
   UNIMPLEMENTED(); 
}


void KeyedLoadICStub::Generate(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void KeyedLoadICStub::GenerateForTrampoline(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void KeyedLoadICStub::GenerateImpl(MacroAssembler* masm, bool in_frame) {
   UNIMPLEMENTED(); 
}


void VectorStoreICTrampolineStub::Generate(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void VectorKeyedStoreICTrampolineStub::Generate(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void VectorStoreICStub::Generate(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void VectorStoreICStub::GenerateForTrampoline(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void VectorStoreICStub::GenerateImpl(MacroAssembler* masm, bool in_frame) {
   UNIMPLEMENTED(); 
}


void VectorKeyedStoreICStub::Generate(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void VectorKeyedStoreICStub::GenerateForTrampoline(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}

/*
static void HandlePolymorphicStoreCase(MacroAssembler* masm, Register feedback,
                                       Register receiver_map, Register scratch1,
                                       Register scratch2, Label* miss) {
   UNIMPLEMENTED(); 
}

*/
void VectorKeyedStoreICStub::GenerateImpl(MacroAssembler* masm, bool in_frame) {
   UNIMPLEMENTED(); 
}


void ProfileEntryHookStub::MaybeCallEntryHook(MacroAssembler* masm) {
   WARNING("ProfileEntryHookStub::MaybeCallEntryHook"); 
}


void ProfileEntryHookStub::Generate(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


template<class T>
static void CreateArrayDispatch(MacroAssembler* masm,
                                AllocationSiteOverrideMode mode) {
   UNIMPLEMENTED(); 
}

/*
static void CreateArrayDispatchOneArgument(MacroAssembler* masm,
                                           AllocationSiteOverrideMode mode) {
   UNIMPLEMENTED(); 
}
*/

template<class T>
static void ArrayConstructorStubAheadOfTimeHelper(Isolate* isolate) {
   UNIMPLEMENTED(); 
}


void ArrayConstructorStubBase::GenerateStubsAheadOfTime(Isolate* isolate) {
   WARNING("ArrayConstructorStubBase::GenerateStubsAheadOfTime");
}


void InternalArrayConstructorStubBase::GenerateStubsAheadOfTime(
    Isolate* isolate) {
    WARNING("InternalArrayConstructorStubBase::GenerateStubsAheadOfTime");
}


void ArrayConstructorStub::GenerateDispatchToArrayStub(
    MacroAssembler* masm,
    AllocationSiteOverrideMode mode) {
   WARNING("ArrayConstructorStub::GenerateDispatchToArrayStub");
}


void ArrayConstructorStub::Generate(MacroAssembler* masm) {
   WARNING("ArrayConstructorStub::Generate");
}


void InternalArrayConstructorStub::GenerateCase(
    MacroAssembler* masm, ElementsKind kind) {
   WARNING("InternalArrayConstructorStub::GenerateCase");
}


void InternalArrayConstructorStub::Generate(MacroAssembler* masm) {
   WARNING("InternalArrayConstructorStub::Generate");
}


void LoadGlobalViaContextStub::Generate(MacroAssembler* masm) {
   WARNING("LoadGlobalViaContextStub::Generate");
}


void StoreGlobalViaContextStub::Generate(MacroAssembler* masm) {
   WARNING("StoreGlobalViaContextStub::Generate");
}
/*

static int AddressOffset(ExternalReference ref0, ExternalReference ref1) {
   UNIMPLEMENTED(); 
}


// Calls an API function.  Allocates HandleScope, extracts returned value
// from handle and propagates exceptions.  Restores context.  stack_space
// - space to be unwound on exit (includes the call JS arguments space and
// the additional space allocated for the fast call).
static void CallApiFunctionAndReturn(
    MacroAssembler* masm, Register function_address,
    ExternalReference thunk_ref, int stack_space, int32_t stack_space_offset,
    MemOperand return_value_operand, MemOperand* context_restore_operand) {
   UNIMPLEMENTED(); 
}


static void CallApiFunctionStubHelper(MacroAssembler* masm,
                                      const ParameterCount& argc,
                                      bool return_first_arg,
                                      bool call_data_undefined) {
   UNIMPLEMENTED(); 
}

*/
void CallApiFunctionStub::Generate(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void CallApiAccessorStub::Generate(MacroAssembler* masm) {
   UNIMPLEMENTED(); 
}


void CallApiGetterStub::Generate(MacroAssembler* masm) {
    UNIMPLEMENTED(); 
}


#undef __
    
    
}  // namespace internal
}  // namespace v8

#endif  // V8_TARGET_ARCH_SPARC
