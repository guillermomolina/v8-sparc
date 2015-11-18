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
//   o ra: return address
//
// The function builds a JS frame.  Please see JavaScriptFrameConstants in
// frames-mips.h for its layout.
void FullCodeGenerator::Generate() {
    WARNING("FullCodeGenerator::Generate");
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
