// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_COMPILER_SPARC_INSTRUCTION_CODES_SPARC_H_
#define V8_COMPILER_SPARC_INSTRUCTION_CODES_SPARC_H_

namespace v8 {
namespace internal {
namespace compiler {

// SPARC-specific opcodes that specify which assembly sequence to emit.
// Most opcodes specify a single instruction.
#define TARGET_ARCH_OPCODE_LIST(V) \
  V(SPARC_And)                       \
  V(SPARC_AndComplement)             \
  V(SPARC_Or)                        \
  V(SPARC_OrComplement)              \
  V(SPARC_Xor)                       \
  V(SPARC_ShiftLeft32)               \
  V(SPARC_ShiftLeft64)               \
  V(SPARC_ShiftRight32)              \
  V(SPARC_ShiftRight64)              \
  V(SPARC_ShiftRightAlg32)           \
  V(SPARC_ShiftRightAlg64)           \
  V(SPARC_RotRight32)                \
  V(SPARC_RotRight64)                \
  V(SPARC_Not)                       \
  V(SPARC_RotLeftAndMask32)          \
  V(SPARC_RotLeftAndClear64)         \
  V(SPARC_RotLeftAndClearLeft64)     \
  V(SPARC_RotLeftAndClearRight64)    \
  V(SPARC_Add)                       \
  V(SPARC_AddWithOverflow32)         \
  V(SPARC_AddDouble)                 \
  V(SPARC_Sub)                       \
  V(SPARC_SubWithOverflow32)         \
  V(SPARC_SubDouble)                 \
  V(SPARC_Mul32)                     \
  V(SPARC_Mul64)                     \
  V(SPARC_MulHigh32)                 \
  V(SPARC_MulHighU32)                \
  V(SPARC_MulDouble)                 \
  V(SPARC_Div32)                     \
  V(SPARC_Div64)                     \
  V(SPARC_DivU32)                    \
  V(SPARC_DivU64)                    \
  V(SPARC_DivDouble)                 \
  V(SPARC_Mod32)                     \
  V(SPARC_Mod64)                     \
  V(SPARC_ModU32)                    \
  V(SPARC_ModU64)                    \
  V(SPARC_ModDouble)                 \
  V(SPARC_Neg)                       \
  V(SPARC_NegDouble)                 \
  V(SPARC_SqrtDouble)                \
  V(SPARC_FloorDouble)               \
  V(SPARC_CeilDouble)                \
  V(SPARC_TruncateDouble)            \
  V(SPARC_RoundDouble)               \
  V(SPARC_MaxDouble)                 \
  V(SPARC_MinDouble)                 \
  V(SPARC_AbsDouble)                 \
  V(SPARC_Cntlz32)                   \
  V(SPARC_Popcnt32)                  \
  V(SPARC_Cmp32)                     \
  V(SPARC_Cmp64)                     \
  V(SPARC_CmpDouble)                 \
  V(SPARC_Tst32)                     \
  V(SPARC_Tst64)                     \
  V(SPARC_Push)                      \
  V(SPARC_PushFrame)                 \
  V(SPARC_StoreToStackSlot)          \
  V(SPARC_ExtendSignWord8)           \
  V(SPARC_ExtendSignWord16)          \
  V(SPARC_ExtendSignWord32)          \
  V(SPARC_Uint32ToUint64)            \
  V(SPARC_Int64ToInt32)              \
  V(SPARC_Int32ToDouble)             \
  V(SPARC_Uint32ToDouble)            \
  V(SPARC_Float32ToDouble)           \
  V(SPARC_DoubleToInt32)             \
  V(SPARC_DoubleToUint32)            \
  V(SPARC_DoubleToFloat32)           \
  V(SPARC_DoubleExtractLowWord32)    \
  V(SPARC_DoubleExtractHighWord32)   \
  V(SPARC_DoubleInsertLowWord32)     \
  V(SPARC_DoubleInsertHighWord32)    \
  V(SPARC_DoubleConstruct)           \
  V(SPARC_BitcastInt32ToFloat32)     \
  V(SPARC_BitcastFloat32ToInt32)     \
  V(SPARC_BitcastInt64ToDouble)      \
  V(SPARC_BitcastDoubleToInt64)      \
  V(SPARC_LoadWordS8)                \
  V(SPARC_LoadWordU8)                \
  V(SPARC_LoadWordS16)               \
  V(SPARC_LoadWordU16)               \
  V(SPARC_LoadWordS32)               \
  V(SPARC_LoadWord64)                \
  V(SPARC_LoadFloat32)               \
  V(SPARC_LoadDouble)                \
  V(SPARC_StoreWord8)                \
  V(SPARC_StoreWord16)               \
  V(SPARC_StoreWord32)               \
  V(SPARC_StoreWord64)               \
  V(SPARC_StoreFloat32)              \
  V(SPARC_StoreDouble)               \
  V(SPARC_StoreWriteBarrier)


// Addressing modes represent the "shape" of inputs to an instruction.
// Many instructions support multiple addressing modes. Addressing modes
// are encoded into the InstructionCode of the instruction and tell the
// code generator after register allocation which assembler method to call.
//
// We use the following local notation for addressing modes:
//
// R = register
// O = register or stack slot
// D = double register
// I = immediate (handle, external, int32)
// MRI = [register + immediate]
// MRR = [register + register]
#define TARGET_ADDRESSING_MODE_LIST(V) \
  V(MRI) /* [%r0 + K] */               \
  V(MRR) /* [%r0 + %r1] */

}  // namespace compiler
}  // namespace internal
}  // namespace v8

#endif  // V8_COMPILER_SPARC_INSTRUCTION_CODES_SPARC_H_
