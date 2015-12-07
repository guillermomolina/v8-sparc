// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/base/adapters.h"
#include "src/compiler/instruction-selector-impl.h"
#include "src/compiler/node-matchers.h"
#include "src/compiler/node-properties.h"
#include "src/sparc/frames-sparc.h"

namespace v8 {
namespace internal {
namespace compiler {

#define TRACE_UNIMPL() \
  PrintF("UNIMPLEMENTED instr_sel: %s at line %d\n", __FUNCTION__, __LINE__)

#define TRACE() PrintF("instr_sel: %s at line %d\n", __FUNCTION__, __LINE__)


/*
static void VisitRR(InstructionSelector* selector, ArchOpcode opcode,
                    Node* node) {
      UNIMPLEMENTED();
}


static void VisitRRR(InstructionSelector* selector, ArchOpcode opcode,
                     Node* node) {
      UNIMPLEMENTED();
}


static void VisitRRO(InstructionSelector* selector, ArchOpcode opcode,
                     Node* node) {
      UNIMPLEMENTED();
}


static void VisitBinop(InstructionSelector* selector, Node* node,
                       InstructionCode opcode, FlagsContinuation* cont) {
      UNIMPLEMENTED();
}


static void VisitBinop(InstructionSelector* selector, Node* node,
                       InstructionCode opcode) {
      UNIMPLEMENTED();
}

*/
void InstructionSelector::VisitLoad(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitStore(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitWord32And(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitWord64And(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitWord32Or(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitWord64Or(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitWord32Xor(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitWord64Xor(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitWord32Shl(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitWord32Shr(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitWord32Sar(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitWord64Shl(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitWord64Shr(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitWord64Sar(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitWord32Ror(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitWord32Clz(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitWord32Ctz(Node* node) { 
      UNIMPLEMENTED();
}

void InstructionSelector::VisitWord64Ctz(Node* node) { 
      UNIMPLEMENTED();
}


void InstructionSelector::VisitWord32Popcnt(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitWord64Popcnt(Node* node){
      UNIMPLEMENTED();
}


void InstructionSelector::VisitWord64Ror(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitWord64Clz(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitInt32Add(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitInt64Add(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitInt32Sub(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitInt64Sub(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitInt32Mul(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitInt32MulHigh(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitUint32MulHigh(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitInt64Mul(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitInt32Div(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitUint32Div(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitInt32Mod(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitUint32Mod(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitInt64Div(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitUint64Div(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitInt64Mod(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitUint64Mod(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitChangeFloat32ToFloat64(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitChangeInt32ToFloat64(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitChangeUint32ToFloat64(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitChangeFloat64ToInt32(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitChangeFloat64ToUint32(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitTruncateFloat32ToInt64(Node* node) {
      UNIMPLEMENTED();
}

void InstructionSelector::VisitTruncateFloat64ToInt64(Node* node) {
      UNIMPLEMENTED();
}

void InstructionSelector::VisitTruncateFloat32ToUint64(Node* node) {
  UNIMPLEMENTED();
}

void InstructionSelector::VisitTruncateFloat64ToUint64(Node* node) {
  UNIMPLEMENTED();
}

void InstructionSelector::VisitChangeInt32ToInt64(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitChangeUint32ToUint64(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitTruncateInt64ToInt32(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitTruncateFloat64ToFloat32(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitTruncateFloat64ToInt32(Node* node) {
      UNIMPLEMENTED();
}

void InstructionSelector::VisitRoundInt64ToFloat32(Node* node) {
      UNIMPLEMENTED();
}

void InstructionSelector::VisitRoundInt64ToFloat64(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitRoundUint64ToFloat64(Node* node) {
  UNIMPLEMENTED();
}


void InstructionSelector::VisitRoundUint64ToFloat32(Node* node) {
  UNIMPLEMENTED();
}


void InstructionSelector::VisitBitcastFloat32ToInt32(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitBitcastFloat64ToInt64(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitBitcastInt32ToFloat32(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitBitcastInt64ToFloat64(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat32Add(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat64Add(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat32Sub(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat64Sub(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat32Mul(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat64Mul(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat32Div(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat64Div(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat64Mod(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat32Max(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat64Max(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat32Min(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat64Min(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat32Abs(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat64Abs(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat32Sqrt(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat64Sqrt(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat32RoundDown(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat64RoundDown(Node* node) {
      UNIMPLEMENTED();
}

void InstructionSelector::VisitFloat32RoundUp(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat64RoundUp(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat32RoundTruncate(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat64RoundTruncate(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat64RoundTiesAway(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat32RoundTiesEven(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat64RoundTiesEven(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::EmitPrepareArguments(NodeVector* arguments,
                                               const CallDescriptor* descriptor,
                                               Node* node) {
      UNIMPLEMENTED();
}


bool InstructionSelector::IsTailCallAddressImmediate() {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitCheckedLoad(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitCheckedStore(Node* node) {
      UNIMPLEMENTED();
}


namespace {
/*
// Shared routine for multiple compare operations.
static void VisitCompare(InstructionSelector* selector, InstructionCode opcode,
                         InstructionOperand left, InstructionOperand right,
                         FlagsContinuation* cont) {
      UNIMPLEMENTED();
}

*/
// Shared routine for multiple float32 compare operations.
void VisitFloat32Compare(InstructionSelector* selector, Node* node,
                         FlagsContinuation* cont) {
      UNIMPLEMENTED();
}


// Shared routine for multiple float64 compare operations.
void VisitFloat64Compare(InstructionSelector* selector, Node* node,
                         FlagsContinuation* cont) {
      UNIMPLEMENTED();
}


// Shared routine for multiple word compare operations.
void VisitWordCompare(InstructionSelector* selector, Node* node,
                      InstructionCode opcode, FlagsContinuation* cont,
                      bool commutative) {
      UNIMPLEMENTED();
}


void VisitWord32Compare(InstructionSelector* selector, Node* node,
                        FlagsContinuation* cont) {
      UNIMPLEMENTED();
}


void VisitWord64Compare(InstructionSelector* selector, Node* node,
                        FlagsContinuation* cont) {
      UNIMPLEMENTED();
}

}  // namespace


void EmitWordCompareZero(InstructionSelector* selector, Node* value,
                         FlagsContinuation* cont) {
      UNIMPLEMENTED();
}


// Shared routine for word comparisons against zero.
void VisitWordCompareZero(InstructionSelector* selector, Node* user,
                          Node* value, FlagsContinuation* cont) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitBranch(Node* branch, BasicBlock* tbranch,
                                      BasicBlock* fbranch) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitSwitch(Node* node, const SwitchInfo& sw) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitWord32Equal(Node* const node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitInt32LessThan(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitInt32LessThanOrEqual(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitUint32LessThan(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitUint32LessThanOrEqual(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitInt32AddWithOverflow(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitInt32SubWithOverflow(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitWord64Equal(Node* const node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitInt64LessThan(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitInt64LessThanOrEqual(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitUint64LessThan(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitUint64LessThanOrEqual(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat32Equal(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat32LessThan(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat32LessThanOrEqual(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat64Equal(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat64LessThan(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat64LessThanOrEqual(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat64ExtractLowWord32(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat64ExtractHighWord32(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat64InsertLowWord32(Node* node) {
      UNIMPLEMENTED();
}


void InstructionSelector::VisitFloat64InsertHighWord32(Node* node) {
      UNIMPLEMENTED();
}


// static
MachineOperatorBuilder::Flags
InstructionSelector::SupportedMachineOperatorFlags() {
      UNIMPLEMENTED();
}

}  // namespace compiler
}  // namespace internal
}  // namespace v8
