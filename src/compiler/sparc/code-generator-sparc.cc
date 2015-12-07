// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/compiler/code-generator.h"

#include "src/compiler/code-generator-impl.h"
#include "src/compiler/gap-resolver.h"
#include "src/compiler/node-matchers.h"
#include "src/compiler/osr.h"
#include "src/sparc/macro-assembler-sparc.h"
#include "src/ast/scopes.h"

namespace v8 {
namespace internal {
namespace compiler {

#define __ masm()->


// TODO(plind): consider renaming these macros.
#define TRACE_MSG(msg)                                                      \
  PrintF("code_gen: \'%s\' in function %s at line %d\n", msg, __FUNCTION__, \
         __LINE__)

#define TRACE_UNIMPL()                                                       \
  PrintF("UNIMPLEMENTED code_generator_sparc: %s at line %d\n", __FUNCTION__, \
         __LINE__)


// Adds Sparc-specific methods to convert InstructionOperands.
class SparcOperandConverter final : public InstructionOperandConverter {
  public:
  SparcOperandConverter(CodeGenerator* gen, Instruction* instr)
      : InstructionOperandConverter(gen, instr) {}

  FloatRegister OutputSingleRegister(size_t index = 0) {
     UNIMPLEMENTED();
  }

  FloatRegister InputSingleRegister(size_t index) {
     UNIMPLEMENTED();
  }

  FloatRegister ToSingleRegister(InstructionOperand* op) {
     UNIMPLEMENTED();
  }

  DoubleRegister InputOrZeroDoubleRegister(size_t index) {
     UNIMPLEMENTED();
  }

  DoubleRegister InputOrZeroSingleRegister(size_t index) {
     UNIMPLEMENTED();
  }

  Operand InputImmediate(size_t index) {
     UNIMPLEMENTED();
  }

  Operand InputOperand(size_t index) {
     UNIMPLEMENTED();
  }

  MemOperand MemoryOperand(size_t* first_index) {
     UNIMPLEMENTED();
  }

  MemOperand MemoryOperand(size_t index = 0) { 
     UNIMPLEMENTED();
  }

  MemOperand ToMemOperand(InstructionOperand* op) const {
     UNIMPLEMENTED();
  }
};


static inline bool HasRegisterInput(Instruction* instr, size_t index) {
     UNIMPLEMENTED();
}


namespace {

class OutOfLineLoadSingle final : public OutOfLineCode {
 public:
  OutOfLineLoadSingle(CodeGenerator* gen, FloatRegister result)
      : OutOfLineCode(gen), result_(result) {}

  void Generate() final {
     UNIMPLEMENTED();
  }

 private:
  FloatRegister const result_;
};


class OutOfLineLoadDouble final : public OutOfLineCode {
 public:
  OutOfLineLoadDouble(CodeGenerator* gen, DoubleRegister result)
      : OutOfLineCode(gen), result_(result) {}

  void Generate() final {
     UNIMPLEMENTED();
  }

 private:
  DoubleRegister const result_;
};


class OutOfLineLoadInteger final : public OutOfLineCode {
 public:
  OutOfLineLoadInteger(CodeGenerator* gen, Register result)
      : OutOfLineCode(gen), result_(result) {}

  void Generate() final { UNIMPLEMENTED(); }

 private:
  Register const result_;
};


class OutOfLineRound : public OutOfLineCode {
 public:
  OutOfLineRound(CodeGenerator* gen, DoubleRegister result)
      : OutOfLineCode(gen), result_(result) {}

  void Generate() final {
     UNIMPLEMENTED();
  }

 private:
  DoubleRegister const result_;
};


class OutOfLineTruncate final : public OutOfLineRound {
 public:
  OutOfLineTruncate(CodeGenerator* gen, DoubleRegister result)
      : OutOfLineRound(gen, result) {}
};


class OutOfLineFloor final : public OutOfLineRound {
 public:
  OutOfLineFloor(CodeGenerator* gen, DoubleRegister result)
      : OutOfLineRound(gen, result) {}
};


class OutOfLineCeil final : public OutOfLineRound {
 public:
  OutOfLineCeil(CodeGenerator* gen, DoubleRegister result)
      : OutOfLineRound(gen, result) {}
};


Condition FlagsConditionToConditionCmp(FlagsCondition condition) {
     UNIMPLEMENTED();
}


Condition FlagsConditionToConditionTst(FlagsCondition condition) {
     UNIMPLEMENTED();
}


Condition FlagsConditionToConditionOvf(FlagsCondition condition) {
     UNIMPLEMENTED();
}


FPUCondition FlagsConditionToConditionCmpFPU(bool& predicate,
                                             FlagsCondition condition) {
     UNIMPLEMENTED();
}

}  // namespace


void CodeGenerator::AssembleDeconstructActivationRecord(int stack_param_delta) {
     UNIMPLEMENTED();
}


// Assembles an instruction after register allocation, producing machine code.
void CodeGenerator::AssembleArchInstruction(Instruction* instr) {
     UNIMPLEMENTED();
}  // NOLINT(readability/fn_size)


#define UNSUPPORTED_COND(opcode, condition)                                  \
  OFStream out(stdout);                                                      \
  out << "Unsupported " << #opcode << " condition: \"" << condition << "\""; \
  UNIMPLEMENTED();

/*static bool convertCondition(FlagsCondition condition, Condition& cc) {
     UNIMPLEMENTED();
}*/


// Assembles branches after an instruction.
void CodeGenerator::AssembleArchBranch(Instruction* instr, BranchInfo* branch) {
     UNIMPLEMENTED();
}


void CodeGenerator::AssembleArchJump(RpoNumber target) {
     UNIMPLEMENTED();
}


// Assembles boolean materializations after an instruction.
void CodeGenerator::AssembleArchBoolean(Instruction* instr,
                                        FlagsCondition condition) {
     UNIMPLEMENTED();
}


void CodeGenerator::AssembleArchLookupSwitch(Instruction* instr) {
     UNIMPLEMENTED();
}


void CodeGenerator::AssembleArchTableSwitch(Instruction* instr) {
     UNIMPLEMENTED();
}


void CodeGenerator::AssembleDeoptimizerCall(
    int deoptimization_id, Deoptimizer::BailoutType bailout_type) {
     UNIMPLEMENTED();
}


void CodeGenerator::AssemblePrologue() {
     UNIMPLEMENTED();
}


void CodeGenerator::AssembleReturn() {
     UNIMPLEMENTED();
}


void CodeGenerator::AssembleMove(InstructionOperand* source,
                                 InstructionOperand* destination) {
     UNIMPLEMENTED();
}


void CodeGenerator::AssembleSwap(InstructionOperand* source,
                                 InstructionOperand* destination) {
     UNIMPLEMENTED();
}


void CodeGenerator::AssembleJumpTable(Label** targets, size_t target_count) {
     UNIMPLEMENTED();
}


void CodeGenerator::AddNopForSmiCodeInlining() {
     UNIMPLEMENTED();
}


void CodeGenerator::EnsureSpaceForLazyDeopt() {
     UNIMPLEMENTED();
}

#undef __

}  // namespace compiler
}  // namespace internal
}  // namespace v8
