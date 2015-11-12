// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#if V8_TARGET_ARCH_SPARC

#include "src/ic/call-optimization.h"
#include "src/ic/handler-compiler.h"
#include "src/ic/ic.h"
#include "src/isolate-inl.h"

namespace v8 {
namespace internal {

#define __ ACCESS_MASM(masm)


void NamedLoadHandlerCompiler::GenerateLoadViaGetter(
    MacroAssembler* masm, Handle<Map> map, Register receiver, Register holder,
    int accessor_index, int expected_arguments, Register scratch) {
    UNIMPLEMENTED();
}


void NamedStoreHandlerCompiler::GenerateStoreViaSetter(
    MacroAssembler* masm, Handle<Map> map, Register receiver, Register holder,
    int accessor_index, int expected_arguments, Register scratch) {
    UNIMPLEMENTED();
}


void PropertyHandlerCompiler::PushVectorAndSlot(Register vector,
                                                Register slot) {
    UNIMPLEMENTED();
}


void PropertyHandlerCompiler::PopVectorAndSlot(Register vector, Register slot) {
    UNIMPLEMENTED();
}


void PropertyHandlerCompiler::DiscardVectorAndSlot() {
    UNIMPLEMENTED();
}


void PropertyHandlerCompiler::GenerateDictionaryNegativeLookup(
    MacroAssembler* masm, Label* miss_label, Register receiver,
    Handle<Name> name, Register scratch0, Register scratch1) {
    UNIMPLEMENTED();
}


void NamedLoadHandlerCompiler::GenerateDirectLoadGlobalFunctionPrototype(
    MacroAssembler* masm, int index, Register result, Label* miss) {
    UNIMPLEMENTED();
}


void NamedLoadHandlerCompiler::GenerateLoadFunctionPrototype(
    MacroAssembler* masm, Register receiver, Register scratch1,
    Register scratch2, Label* miss_label) {
    UNIMPLEMENTED();
}


// Generate code to check that a global property cell is empty. Create
// the property cell at compilation time if no cell exists for the
// property.
void PropertyHandlerCompiler::GenerateCheckPropertyCell(
    MacroAssembler* masm, Handle<JSGlobalObject> global, Handle<Name> name,
    Register scratch, Label* miss) {
    UNIMPLEMENTED();
}

/*
static void PushInterceptorArguments(MacroAssembler* masm, Register receiver,
                                     Register holder, Register name,
                                     Handle<JSObject> holder_obj) {
    UNIMPLEMENTED();
}


static void CompileCallLoadPropertyWithInterceptor(
    MacroAssembler* masm, Register receiver, Register holder, Register name,
    Handle<JSObject> holder_obj, Runtime::FunctionId id) {
    UNIMPLEMENTED();
}
*/

// Generate call to api function.
void PropertyHandlerCompiler::GenerateApiAccessorCall(
    MacroAssembler* masm, const CallOptimization& optimization,
    Handle<Map> receiver_map, Register receiver, Register scratch_in,
    bool is_store, Register store_parameter, Register accessor_holder,
    int accessor_index) {
    UNIMPLEMENTED();
}

/*
static void StoreIC_PushArgs(MacroAssembler* masm) {
    UNIMPLEMENTED();
}
*/

void NamedStoreHandlerCompiler::GenerateSlow(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


void ElementHandlerCompiler::GenerateStoreSlow(MacroAssembler* masm) {
    UNIMPLEMENTED();
}


#undef __
#define __ ACCESS_MASM(masm())


void NamedStoreHandlerCompiler::GenerateRestoreName(Label* label,
                                                    Handle<Name> name) {
    UNIMPLEMENTED();
}


void NamedStoreHandlerCompiler::GenerateRestoreName(Handle<Name> name) {
    UNIMPLEMENTED();
}


void NamedStoreHandlerCompiler::RearrangeVectorAndSlot(
    Register current_map, Register destination_map) {
    UNIMPLEMENTED();
}


void NamedStoreHandlerCompiler::GenerateRestoreMap(Handle<Map> transition,
                                                   Register map_reg,
                                                   Register scratch,
                                                   Label* miss) {
    UNIMPLEMENTED();
}


void NamedStoreHandlerCompiler::GenerateConstantCheck(Register map_reg,
                                                      int descriptor,
                                                      Register value_reg,
                                                      Register scratch,
                                                      Label* miss_label) {
    UNIMPLEMENTED();
}


void NamedStoreHandlerCompiler::GenerateFieldTypeChecks(HeapType* field_type,
                                                        Register value_reg,
                                                        Label* miss_label) {
    UNIMPLEMENTED();
}


Register PropertyHandlerCompiler::CheckPrototypes(
    Register object_reg, Register holder_reg, Register scratch1,
    Register scratch2, Handle<Name> name, Label* miss, PrototypeCheckType check,
    ReturnHolder return_what) {
     UNIMPLEMENTED();
}


void NamedLoadHandlerCompiler::FrontendFooter(Handle<Name> name, Label* miss) {
    UNIMPLEMENTED();
}


void NamedStoreHandlerCompiler::FrontendFooter(Handle<Name> name, Label* miss) {
    UNIMPLEMENTED();
}


void NamedLoadHandlerCompiler::GenerateLoadConstant(Handle<Object> value) {
    UNIMPLEMENTED();
}


void NamedLoadHandlerCompiler::GenerateLoadCallback(
    Register reg, Handle<ExecutableAccessorInfo> callback) {
    UNIMPLEMENTED();
}


void NamedLoadHandlerCompiler::GenerateLoadInterceptorWithFollowup(
    LookupIterator* it, Register holder_reg) {
    UNIMPLEMENTED();
}


void NamedLoadHandlerCompiler::GenerateLoadInterceptor(Register holder_reg) {
    UNIMPLEMENTED();
}


Handle<Code> NamedStoreHandlerCompiler::CompileStoreCallback(
    Handle<JSObject> object, Handle<Name> name,
    Handle<ExecutableAccessorInfo> callback) {
    UNIMPLEMENTED();
}


Handle<Code> NamedStoreHandlerCompiler::CompileStoreInterceptor(
    Handle<Name> name) {
    UNIMPLEMENTED();
}


Register NamedStoreHandlerCompiler::value() {
    UNIMPLEMENTED();
}


Handle<Code> NamedLoadHandlerCompiler::CompileLoadGlobal(
    Handle<PropertyCell> cell, Handle<Name> name, bool is_configurable) {
    UNIMPLEMENTED();
}


#undef __
}  // namespace internal
}  // namespace v8

#endif  // V8_TARGET_ARCH_ARM
