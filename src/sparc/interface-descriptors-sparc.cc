// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#if V8_TARGET_ARCH_SPARC

#include "src/interface-descriptors.h"

namespace v8 {
namespace internal {

    
const Register CallInterfaceDescriptor::ContextRegister() { UNIMPLEMENTED(); }


const Register LoadDescriptor::ReceiverRegister() { UNIMPLEMENTED(); }
const Register LoadDescriptor::NameRegister() { UNIMPLEMENTED(); }
const Register LoadDescriptor::SlotRegister()  { UNIMPLEMENTED(); }


const Register LoadWithVectorDescriptor::VectorRegister()  { UNIMPLEMENTED(); }


const Register StoreDescriptor::ReceiverRegister()  { UNIMPLEMENTED(); }
const Register StoreDescriptor::NameRegister()  { UNIMPLEMENTED(); }
const Register StoreDescriptor::ValueRegister()  { UNIMPLEMENTED(); }


const Register VectorStoreICTrampolineDescriptor::SlotRegister()  { UNIMPLEMENTED(); }


const Register VectorStoreICDescriptor::VectorRegister()  { UNIMPLEMENTED(); }


const Register VectorStoreTransitionDescriptor::SlotRegister()  { UNIMPLEMENTED(); }
const Register VectorStoreTransitionDescriptor::VectorRegister()  { UNIMPLEMENTED(); }
const Register VectorStoreTransitionDescriptor::MapRegister()  { UNIMPLEMENTED(); }


const Register StoreTransitionDescriptor::MapRegister()  { UNIMPLEMENTED(); }


const Register LoadGlobalViaContextDescriptor::SlotRegister()  { UNIMPLEMENTED(); }

const Register StoreGlobalViaContextDescriptor::SlotRegister() { UNIMPLEMENTED(); }
const Register StoreGlobalViaContextDescriptor::ValueRegister() { UNIMPLEMENTED(); }


const Register InstanceOfDescriptor::LeftRegister()  { UNIMPLEMENTED(); }
const Register InstanceOfDescriptor::RightRegister() { UNIMPLEMENTED(); }


const Register StringCompareDescriptor::LeftRegister()  { UNIMPLEMENTED(); }
const Register StringCompareDescriptor::RightRegister()  { UNIMPLEMENTED(); }


const Register ArgumentsAccessReadDescriptor::index()  { UNIMPLEMENTED(); }
const Register ArgumentsAccessReadDescriptor::parameter_count()  { UNIMPLEMENTED(); }


const Register ArgumentsAccessNewDescriptor::function()  { UNIMPLEMENTED(); }
const Register ArgumentsAccessNewDescriptor::parameter_count()  { UNIMPLEMENTED(); }
const Register ArgumentsAccessNewDescriptor::parameter_pointer()  { UNIMPLEMENTED(); }


const Register ApiGetterDescriptor::function_address()  { UNIMPLEMENTED(); }


const Register MathPowTaggedDescriptor::exponent()  { UNIMPLEMENTED(); }


const Register MathPowIntegerDescriptor::exponent()  { UNIMPLEMENTED(); }


const Register GrowArrayElementsDescriptor::ObjectRegister() { UNIMPLEMENTED(); }
const Register GrowArrayElementsDescriptor::KeyRegister()  { UNIMPLEMENTED(); }


void FastNewClosureDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
  UNIMPLEMENTED();
}


void FastNewContextDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
  UNIMPLEMENTED();
}


void ToNumberDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
  UNIMPLEMENTED();
}


// static
const Register ToLengthDescriptor::ReceiverRegister()  { UNIMPLEMENTED(); }


// static
const Register ToStringDescriptor::ReceiverRegister()   { UNIMPLEMENTED(); }


// static
const Register ToObjectDescriptor::ReceiverRegister()  { UNIMPLEMENTED(); }


void NumberToStringDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void TypeofDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void FastCloneRegExpDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void FastCloneShallowArrayDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void FastCloneShallowObjectDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void CreateAllocationSiteDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void CreateWeakCellDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void StoreArrayLiteralElementDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void CallFunctionDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void CallFunctionWithFeedbackDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void CallFunctionWithFeedbackAndVectorDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void CallConstructDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void CallTrampolineDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void ConstructTrampolineDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void RegExpConstructResultDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void TransitionElementsKindDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void AllocateHeapNumberDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void AllocateInNewSpaceDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void ArrayConstructorConstantArgCountDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void ArrayConstructorDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void InternalArrayConstructorConstantArgCountDescriptor::
    InitializePlatformSpecific(CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void InternalArrayConstructorDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void CompareDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void CompareNilDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void ToBooleanDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void BinaryOpDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void BinaryOpWithAllocationSiteDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void StringAddDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void KeyedDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void NamedDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void CallHandlerDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void ArgumentAdaptorDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void ApiFunctionDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void ApiAccessorDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void MathRoundVariantCallFromUnoptimizedCodeDescriptor::
    InitializePlatformSpecific(CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void MathRoundVariantCallFromOptimizedCodeDescriptor::
    InitializePlatformSpecific(CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void InterpreterPushArgsAndCallDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void InterpreterPushArgsAndConstructDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}


void InterpreterCEntryDescriptor::InitializePlatformSpecific(
    CallInterfaceDescriptorData* data) {
 UNIMPLEMENTED();
}

}  // namespace internal
}  // namespace v8

#endif  // V8_TARGET_ARCH_SPARC
