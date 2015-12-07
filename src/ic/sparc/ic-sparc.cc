// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#if V8_TARGET_ARCH_SPARC

#include "src/codegen.h"
#include "src/ic/ic.h"
#include "src/ic/ic-compiler.h"
#include "src/ic/stub-cache.h"

namespace v8 {
namespace internal {



// ----------------------------------------------------------------------------
// Static IC stub generators.
//

#define __ ACCESS_MASM(masm)


/*static void GenerateGlobalInstanceTypeCheck(MacroAssembler* masm, Register type,
                                            Label* global_object) {
     WARNING("Builtins::");
}*/

/*static void GenerateDictionaryLoad(MacroAssembler* masm, Label* miss,
                                   Register elements, Register name,
                                   Register result, Register scratch1,
                                   Register scratch2) {
     WARNING("Builtins::");
}*/

/*static void GenerateDictionaryStore(MacroAssembler* masm, Label* miss,
                                    Register elements, Register name,
                                    Register value, Register scratch1,
                                    Register scratch2) {
     WARNING("Builtins::");
}*/


// Checks the receiver for special cases (value type, slow case bits).
// Falls through for regular JS object.
/*static void GenerateKeyedLoadReceiverCheck(MacroAssembler* masm,
                                           Register receiver, Register map,
                                           Register scratch,
                                           int interceptor_bit, Label* slow) {
     WARNING("Builtins::");
}*/


// Loads an indexed element from a fast case array.
/*static void GenerateFastArrayLoad(MacroAssembler* masm, Register receiver,
                                  Register key, Register elements,
                                  Register scratch1, Register scratch2,
                                  Register result, Label* slow,
                                  LanguageMode language_mode) {
     WARNING("Builtins::");
}*/


// Checks whether a key is an array index string or a unique name.
// Falls through if a key is a unique name.
/*static void GenerateKeyNameCheck(MacroAssembler* masm, Register key,
                                 Register map, Register hash,
                                 Label* index_string, Label* not_unique) {
     WARNING("Builtins::");
}*/


void LoadIC::GenerateNormal(MacroAssembler* masm, LanguageMode language_mode) {
     WARNING("LoadIC::GenerateNormal");
    __ breakpoint_trap();
}


// A register that isn't one of the parameters to the load ic.
/*static const Register LoadIC_TempRegister() { 
     WARNING("Builtins::");
}*/


/*static void LoadIC_PushArgs(MacroAssembler* masm) {
     WARNING("Builtins::");
}*/


void LoadIC::GenerateMiss(MacroAssembler* masm) {
     WARNING("LoadIC::GenerateMiss");
    __ breakpoint_trap();
}


void LoadIC::GenerateRuntimeGetProperty(MacroAssembler* masm,
                                        LanguageMode language_mode) {
     WARNING("LoadIC::GenerateRuntimeGetProperty");
    __ breakpoint_trap();
}


void KeyedLoadIC::GenerateMiss(MacroAssembler* masm) {
     WARNING("KeyedLoadIC::GenerateMiss");
    __ breakpoint_trap();
}


void KeyedLoadIC::GenerateRuntimeGetProperty(MacroAssembler* masm,
                                             LanguageMode language_mode) {
     WARNING("KeyedLoadIC::GenerateRuntimeGetProperty");
    __ breakpoint_trap();
}


void KeyedLoadIC::GenerateMegamorphic(MacroAssembler* masm,
                                      LanguageMode language_mode) {
     WARNING("KeyedLoadIC::GenerateMegamorphic");
    __ breakpoint_trap();
}


/*static void KeyedStoreGenerateMegamorphicHelper(
    MacroAssembler* masm, Label* fast_object, Label* fast_double, Label* slow,
    KeyedStoreCheckMap check_map, KeyedStoreIncrementLength increment_length,
    Register value, Register key, Register receiver, Register receiver_map,
    Register elements_map, Register elements) {
     WARNING("Builtins::");
}*/


void KeyedStoreIC::GenerateMegamorphic(MacroAssembler* masm,
                                       LanguageMode language_mode) {
     WARNING("KeyedStoreIC::GenerateMegamorphic");
    __ breakpoint_trap();
}


/*static void StoreIC_PushArgs(MacroAssembler* masm) {
     WARNING("Builtins::");
}*/


void KeyedStoreIC::GenerateMiss(MacroAssembler* masm) {
     WARNING("KeyedStoreIC::GenerateMiss");
__ breakpoint_trap();
}


void StoreIC::GenerateMegamorphic(MacroAssembler* masm) {
     WARNING("StoreIC::GenerateMegamorphic");
    __ breakpoint_trap();
}


void StoreIC::GenerateMiss(MacroAssembler* masm) {
     WARNING("StoreIC::GenerateMiss");
    __ breakpoint_trap();
}


void StoreIC::GenerateNormal(MacroAssembler* masm) {
     WARNING("StoreIC::GenerateNormal");
    __ breakpoint_trap();
}


#undef __


Condition CompareIC::ComputeCondition(Token::Value op) {
     WARNING("CompareIC::ComputeCondition");
     return always;
}


bool CompareIC::HasInlinedSmiCode(Address address) {
     WARNING("CompareIC::HasInlinedSmiCode");
     return false;
}

void PatchInlinedSmiCode(Isolate* isolate, Address address,
                         InlinedSmiCheck check) {
         WARNING("PatchInlinedSmiCode");
}

void PatchInlinedSmiCode(Address address, InlinedSmiCheck check) {
     WARNING("PatchInlinedSmiCode");
}



}  // namespace internal
}  // namespace v8

#endif  // V8_TARGET_ARCH_SPARC
