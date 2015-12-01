// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/crankshaft/sparc/lithium-codegen-sparc.h"

#include "src/base/bits.h"
#include "src/code-factory.h"
#include "src/code-stubs.h"
#include "src/crankshaft/hydrogen-osr.h"
#include "src/crankshaft/sparc/lithium-gap-resolver-sparc.h"
#include "src/ic/ic.h"
#include "src/ic/stub-cache.h"
#include "src/profiler/cpu-profiler.h"

namespace v8 {
namespace internal {

 
class SafepointGenerator final : public CallWrapper {
 public:
  SafepointGenerator(LCodeGen* codegen,
                     LPointerMap* pointers,
                     Safepoint::DeoptMode mode)
      : codegen_(codegen),
        pointers_(pointers),
        deopt_mode_(mode) { }
  virtual ~SafepointGenerator() {}

  void BeforeCall(int call_size) const override {}

  void AfterCall() const override {
    codegen_->RecordSafepoint(pointers_, deopt_mode_);
  }

 private:
  LCodeGen* codegen_;
  LPointerMap* pointers_;
  Safepoint::DeoptMode deopt_mode_;
};


#define __ masm()->

bool LCodeGen::GenerateCode() {
    UNIMPLEMENTED();
}


void LCodeGen::FinishCode(Handle<Code> code) {
    UNIMPLEMENTED();
}


void LCodeGen::SaveCallerDoubles() {
    UNIMPLEMENTED();
}


void LCodeGen::RestoreCallerDoubles() {
    UNIMPLEMENTED();
}


bool LCodeGen::GeneratePrologue() {
    UNIMPLEMENTED();
}


void LCodeGen::DoPrologue(LPrologue* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::GenerateOsrPrologue() {
    UNIMPLEMENTED();
}


void LCodeGen::GenerateBodyInstructionPre(LInstruction* instr) {
    UNIMPLEMENTED();
}


bool LCodeGen::GenerateDeferredCode() {
    UNIMPLEMENTED();
}


bool LCodeGen::GenerateJumpTable() {
    UNIMPLEMENTED();
}


bool LCodeGen::GenerateSafepointTable() {
    UNIMPLEMENTED();
}


Register LCodeGen::ToRegister(int index) const {
    UNIMPLEMENTED();
}


DoubleRegister LCodeGen::ToDoubleRegister(int index) const {
    UNIMPLEMENTED();
}


Register LCodeGen::ToRegister(LOperand* op) const {
    UNIMPLEMENTED();
}


Register LCodeGen::EmitLoadRegister(LOperand* op, Register scratch) {
    UNIMPLEMENTED();
}


DoubleRegister LCodeGen::ToDoubleRegister(LOperand* op) const {
    UNIMPLEMENTED();
}


DoubleRegister LCodeGen::EmitLoadDoubleRegister(LOperand* op,
                                                FloatRegister flt_scratch,
                                                DoubleRegister dbl_scratch) {
    UNIMPLEMENTED();
}


Handle<Object> LCodeGen::ToHandle(LConstantOperand* op) const {
    UNIMPLEMENTED();
}


bool LCodeGen::IsInteger32(LConstantOperand* op) const {
    UNIMPLEMENTED();
}


bool LCodeGen::IsSmi(LConstantOperand* op) const {
    UNIMPLEMENTED();
}


int32_t LCodeGen::ToInteger32(LConstantOperand* op) const {
    UNIMPLEMENTED();
}


int64_t LCodeGen::ToRepresentation_donotuse(LConstantOperand* op,
                                            const Representation& r) const {
    UNIMPLEMENTED();
}


Smi* LCodeGen::ToSmi(LConstantOperand* op) const {
    UNIMPLEMENTED();
}


double LCodeGen::ToDouble(LConstantOperand* op) const {
    UNIMPLEMENTED();
}


Operand LCodeGen::ToOperand(LOperand* op) {
    UNIMPLEMENTED();
}



MemOperand LCodeGen::ToMemOperand(LOperand* op) const {
    UNIMPLEMENTED();
}


MemOperand LCodeGen::ToHighMemOperand(LOperand* op) const {
    UNIMPLEMENTED();
}


void LCodeGen::WriteTranslation(LEnvironment* environment,
                                Translation* translation) {
    UNIMPLEMENTED();
}


void LCodeGen::AddToTranslation(LEnvironment* environment,
                                Translation* translation,
                                LOperand* op,
                                bool is_tagged,
                                bool is_uint32,
                                int* object_index_pointer,
                                int* dematerialized_index_pointer) {
    UNIMPLEMENTED();
}


void LCodeGen::CallCode(Handle<Code> code,
                        RelocInfo::Mode mode,
                        LInstruction* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::CallCodeGeneric(Handle<Code> code,
                               RelocInfo::Mode mode,
                               LInstruction* instr,
                               SafepointMode safepoint_mode) {
    UNIMPLEMENTED();
}


void LCodeGen::CallRuntime(const Runtime::Function* function,
                           int num_arguments,
                           LInstruction* instr,
                           SaveFPRegsMode save_doubles) {
    UNIMPLEMENTED();
}


void LCodeGen::LoadContextFromDeferred(LOperand* context) {
    UNIMPLEMENTED();
}


void LCodeGen::CallRuntimeFromDeferred(Runtime::FunctionId id,
                                       int argc,
                                       LInstruction* instr,
                                       LOperand* context) {
    UNIMPLEMENTED();
}


void LCodeGen::RegisterEnvironmentForDeoptimization(LEnvironment* environment,
                                                    Safepoint::DeoptMode mode) {
    UNIMPLEMENTED();
}


void LCodeGen::DeoptimizeIf(Condition condition, LInstruction* instr,
                            Deoptimizer::DeoptReason deopt_reason,
                            Deoptimizer::BailoutType bailout_type,
                            Register src1, const Operand& src2) {
    UNIMPLEMENTED();
}


void LCodeGen::DeoptimizeIf(Condition condition, LInstruction* instr,
                            Deoptimizer::DeoptReason deopt_reason,
                            Register src1, const Operand& src2) {
    UNIMPLEMENTED();
}


void LCodeGen::PopulateDeoptimizationData(Handle<Code> code) {
    UNIMPLEMENTED();
}


void LCodeGen::PopulateDeoptimizationLiteralsWithInlinedFunctions() {
    UNIMPLEMENTED();
}


void LCodeGen::RecordSafepointWithLazyDeopt(
    LInstruction* instr, SafepointMode safepoint_mode) {
    UNIMPLEMENTED();
}


void LCodeGen::RecordSafepoint(
    LPointerMap* pointers,
    Safepoint::Kind kind,
    int arguments,
    Safepoint::DeoptMode deopt_mode) {
    UNIMPLEMENTED();
}


void LCodeGen::RecordSafepoint(LPointerMap* pointers,
                               Safepoint::DeoptMode deopt_mode) {
    UNIMPLEMENTED();
}


void LCodeGen::RecordSafepoint(Safepoint::DeoptMode deopt_mode) {
    UNIMPLEMENTED();
}


void LCodeGen::RecordSafepointWithRegisters(LPointerMap* pointers,
                                            int arguments,
                                            Safepoint::DeoptMode deopt_mode) {
    UNIMPLEMENTED();
}


void LCodeGen::RecordAndWritePosition(int position) {
    UNIMPLEMENTED();
}



void LCodeGen::DoLabel(LLabel* label) {
    UNIMPLEMENTED();
}


void LCodeGen::DoParallelMove(LParallelMove* move) {
    UNIMPLEMENTED();
}


void LCodeGen::DoGap(LGap* gap) {
    UNIMPLEMENTED();
}


void LCodeGen::DoInstructionGap(LInstructionGap* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoParameter(LParameter* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoCallStub(LCallStub* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoUnknownOSRValue(LUnknownOSRValue* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoModByPowerOf2I(LModByPowerOf2I* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoModByConstI(LModByConstI* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoModI(LModI* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoDivByPowerOf2I(LDivByPowerOf2I* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoDivByConstI(LDivByConstI* instr) {
    UNIMPLEMENTED();
}


// TODO(svenpanne) Refactor this to avoid code duplication with DoFlooringDivI.
void LCodeGen::DoDivI(LDivI* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoMultiplyAddD(LMultiplyAddD* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoFlooringDivByPowerOf2I(LFlooringDivByPowerOf2I* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoFlooringDivByConstI(LFlooringDivByConstI* instr) {
    UNIMPLEMENTED();
}


// TODO(svenpanne) Refactor this to avoid code duplication with DoDivI.
void LCodeGen::DoFlooringDivI(LFlooringDivI* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoMulS(LMulS* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoMulI(LMulI* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoBitI(LBitI* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoShiftI(LShiftI* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoSubS(LSubS* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoSubI(LSubI* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoConstantI(LConstantI* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoConstantS(LConstantS* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoConstantD(LConstantD* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoConstantE(LConstantE* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoConstantT(LConstantT* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoMapEnumLength(LMapEnumLength* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoDateField(LDateField* instr) {
    UNIMPLEMENTED();
}


MemOperand LCodeGen::BuildSeqStringOperand(Register string,
                                           LOperand* index,
                                           String::Encoding encoding) {
    UNIMPLEMENTED();
}


void LCodeGen::DoSeqStringGetChar(LSeqStringGetChar* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoSeqStringSetChar(LSeqStringSetChar* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoAddE(LAddE* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoAddS(LAddS* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoAddI(LAddI* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoMathMinMax(LMathMinMax* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoArithmeticD(LArithmeticD* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoArithmeticT(LArithmeticT* instr) {
    UNIMPLEMENTED();
}


template<class InstrType>
void LCodeGen::EmitBranch(InstrType instr,
                          Condition condition,
                          Register src1,
                          const Operand& src2) {
    UNIMPLEMENTED();
}


template<class InstrType>
void LCodeGen::EmitBranchF(InstrType instr,
                           Condition condition,
                           FPURegister src1,
                           FPURegister src2) {
    UNIMPLEMENTED();
}


template <class InstrType>
void LCodeGen::EmitTrueBranch(InstrType instr, Condition condition,
                              Register src1, const Operand& src2) {
    UNIMPLEMENTED();
}


template <class InstrType>
void LCodeGen::EmitFalseBranch(InstrType instr, Condition condition,
                               Register src1, const Operand& src2) {
    UNIMPLEMENTED();
}


template<class InstrType>
void LCodeGen::EmitFalseBranchF(InstrType instr,
                                Condition condition,
                                FPURegister src1,
                                FPURegister src2) {
    UNIMPLEMENTED();
}


void LCodeGen::DoDebugBreak(LDebugBreak* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoBranch(LBranch* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::EmitGoto(int block) {
    UNIMPLEMENTED();
}


void LCodeGen::DoGoto(LGoto* instr) {
    UNIMPLEMENTED();
}


Condition LCodeGen::TokenToCondition(Token::Value op, bool is_unsigned) {
    UNIMPLEMENTED();
}


void LCodeGen::DoCompareNumericAndBranch(LCompareNumericAndBranch* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoCmpObjectEqAndBranch(LCmpObjectEqAndBranch* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoCmpHoleAndBranch(LCmpHoleAndBranch* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoCompareMinusZeroAndBranch(LCompareMinusZeroAndBranch* instr) {
    UNIMPLEMENTED();
}


Condition LCodeGen::EmitIsString(Register input,
                                 Register temp1,
                                 Label* is_not_string,
                                 SmiCheck check_needed = INLINE_SMI_CHECK) {
    UNIMPLEMENTED();
}


void LCodeGen::DoIsStringAndBranch(LIsStringAndBranch* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoIsSmiAndBranch(LIsSmiAndBranch* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoIsUndetectableAndBranch(LIsUndetectableAndBranch* instr) {
    UNIMPLEMENTED();
}



void LCodeGen::DoStringCompareAndBranch(LStringCompareAndBranch* instr) {
    UNIMPLEMENTED();
}




void LCodeGen::DoHasInstanceTypeAndBranch(LHasInstanceTypeAndBranch* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoGetCachedArrayIndex(LGetCachedArrayIndex* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoHasCachedArrayIndexAndBranch(
    LHasCachedArrayIndexAndBranch* instr) {
    UNIMPLEMENTED();
}


// Branches to a label or falls through with the answer in flags.  Trashes
// the temp registers, but not the input.
void LCodeGen::EmitClassOfTest(Label* is_true,
                               Label* is_false,
                               Handle<String>class_name,
                               Register input,
                               Register temp,
                               Register temp2) {
    UNIMPLEMENTED();
}


void LCodeGen::DoClassOfTestAndBranch(LClassOfTestAndBranch* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoCmpMapAndBranch(LCmpMapAndBranch* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoInstanceOf(LInstanceOf* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoHasInPrototypeChainAndBranch(
    LHasInPrototypeChainAndBranch* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoCmpT(LCmpT* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoReturn(LReturn* instr) {
    UNIMPLEMENTED();
}


template <class T>
void LCodeGen::EmitVectorLoadICRegisters(T* instr) {
    UNIMPLEMENTED();
}


template <class T>
void LCodeGen::EmitVectorStoreICRegisters(T* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoLoadGlobalGeneric(LLoadGlobalGeneric* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoLoadContextSlot(LLoadContextSlot* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoStoreContextSlot(LStoreContextSlot* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoLoadNamedField(LLoadNamedField* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoLoadNamedGeneric(LLoadNamedGeneric* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoLoadFunctionPrototype(LLoadFunctionPrototype* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoLoadRoot(LLoadRoot* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoAccessArgumentsAt(LAccessArgumentsAt* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoLoadKeyedExternalArray(LLoadKeyed* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoLoadKeyedFixedDoubleArray(LLoadKeyed* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoLoadKeyedFixedArray(LLoadKeyed* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoLoadKeyed(LLoadKeyed* instr) {
    UNIMPLEMENTED();
}


MemOperand LCodeGen::PrepareKeyedOperand(Register key,
                                         Register base,
                                         bool key_is_constant,
                                         int constant_key,
                                         int element_size,
                                         int shift_size,
                                         int base_offset) {
    UNIMPLEMENTED();
}


void LCodeGen::DoLoadKeyedGeneric(LLoadKeyedGeneric* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoArgumentsElements(LArgumentsElements* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoArgumentsLength(LArgumentsLength* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoWrapReceiver(LWrapReceiver* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoApplyArguments(LApplyArguments* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoPushArgument(LPushArgument* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoDrop(LDrop* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoThisFunction(LThisFunction* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoContext(LContext* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoDeclareGlobals(LDeclareGlobals* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::CallKnownFunction(Handle<JSFunction> function,
                                 int formal_parameter_count, int arity,
                                 LInstruction* instr) {
    UNIMPLEMENTED();
}

/*
void LCodeGen::DoDeferredMathAbsTaggedHeapNumber(LMathAbs* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::EmitIntegerMathAbs(LMathAbs* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::EmitSmiMathAbs(LMathAbs* instr) {
    UNIMPLEMENTED();
}
*/

void LCodeGen::DoMathAbs(LMathAbs* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoMathFloor(LMathFloor* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoMathRound(LMathRound* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoMathFround(LMathFround* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoMathSqrt(LMathSqrt* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoMathPowHalf(LMathPowHalf* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoPower(LPower* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoMathExp(LMathExp* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoMathLog(LMathLog* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoMathClz32(LMathClz32* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoInvokeFunction(LInvokeFunction* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoCallWithDescriptor(LCallWithDescriptor* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoCallJSFunction(LCallJSFunction* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoCallFunction(LCallFunction* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoCallNewArray(LCallNewArray* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoCallRuntime(LCallRuntime* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoStoreCodeEntry(LStoreCodeEntry* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoInnerAllocatedObject(LInnerAllocatedObject* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoStoreNamedField(LStoreNamedField* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoStoreNamedGeneric(LStoreNamedGeneric* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoBoundsCheck(LBoundsCheck* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoStoreKeyedExternalArray(LStoreKeyed* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoStoreKeyedFixedDoubleArray(LStoreKeyed* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoStoreKeyedFixedArray(LStoreKeyed* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoStoreKeyed(LStoreKeyed* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoStoreKeyedGeneric(LStoreKeyedGeneric* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoMaybeGrowElements(LMaybeGrowElements* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoDeferredMaybeGrowElements(LMaybeGrowElements* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoTransitionElementsKind(LTransitionElementsKind* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoTrapAllocationMemento(LTrapAllocationMemento* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoStringAdd(LStringAdd* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoStringCharCodeAt(LStringCharCodeAt* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoDeferredStringCharCodeAt(LStringCharCodeAt* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoStringCharFromCode(LStringCharFromCode* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoDeferredStringCharFromCode(LStringCharFromCode* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoInteger32ToDouble(LInteger32ToDouble* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoUint32ToDouble(LUint32ToDouble* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoNumberTagU(LNumberTagU* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoDeferredNumberTagIU(LInstruction* instr,
                                     LOperand* value,
                                     LOperand* temp1,
                                     LOperand* temp2,
                                     IntegerSignedness signedness) {
    UNIMPLEMENTED();
}


void LCodeGen::DoNumberTagD(LNumberTagD* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoDeferredNumberTagD(LNumberTagD* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoSmiTag(LSmiTag* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoSmiUntag(LSmiUntag* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::EmitNumberUntagD(LNumberUntagD* instr, Register input_reg,
                                DoubleRegister result_reg,
                                NumberUntagDMode mode) {
    UNIMPLEMENTED();
}


void LCodeGen::DoDeferredTaggedToI(LTaggedToI* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoTaggedToI(LTaggedToI* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoNumberUntagD(LNumberUntagD* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoDoubleToI(LDoubleToI* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoDoubleToSmi(LDoubleToSmi* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoCheckSmi(LCheckSmi* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoCheckNonSmi(LCheckNonSmi* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoCheckArrayBufferNotNeutered(
    LCheckArrayBufferNotNeutered* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoCheckInstanceType(LCheckInstanceType* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoCheckValue(LCheckValue* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoDeferredInstanceMigration(LCheckMaps* instr, Register object) {
    UNIMPLEMENTED();
}


void LCodeGen::DoCheckMaps(LCheckMaps* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoClampDToUint8(LClampDToUint8* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoClampIToUint8(LClampIToUint8* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoClampTToUint8(LClampTToUint8* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoDoubleBits(LDoubleBits* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoConstructDouble(LConstructDouble* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoAllocate(LAllocate* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoDeferredAllocate(LAllocate* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoToFastProperties(LToFastProperties* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoTypeof(LTypeof* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoTypeofIsAndBranch(LTypeofIsAndBranch* instr) {
    UNIMPLEMENTED();
}


Condition LCodeGen::EmitTypeofIs(Label* true_label,
                                 Label* false_label,
                                 Register input,
                                 Handle<String> type_name,
                                 Register* cmp1,
                                 Operand* cmp2) {
    UNIMPLEMENTED();
}


void LCodeGen::DoIsConstructCallAndBranch(LIsConstructCallAndBranch* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::EmitIsConstructCall(Register temp1, Register temp2) {
    UNIMPLEMENTED();
}


void LCodeGen::EnsureSpaceForLazyDeopt(int space_needed) {
    UNIMPLEMENTED();
}


void LCodeGen::DoLazyBailout(LLazyBailout* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoDeoptimize(LDeoptimize* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoDummy(LDummy* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoDummyUse(LDummyUse* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoDeferredStackCheck(LStackCheck* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoStackCheck(LStackCheck* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoOsrEntry(LOsrEntry* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoForInPrepareMap(LForInPrepareMap* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoForInCacheArray(LForInCacheArray* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoCheckMapValue(LCheckMapValue* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoDeferredLoadMutableDouble(LLoadFieldByIndex* instr,
                                           Register result,
                                           Register object,
                                           Register index) {
    UNIMPLEMENTED();
}


void LCodeGen::DoLoadFieldByIndex(LLoadFieldByIndex* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoStoreFrameContext(LStoreFrameContext* instr) {
    UNIMPLEMENTED();
}


void LCodeGen::DoAllocateBlockContext(LAllocateBlockContext* instr) {
    UNIMPLEMENTED();
}


#undef __
   
    
}  // namespace internal
}  // namespace v8
