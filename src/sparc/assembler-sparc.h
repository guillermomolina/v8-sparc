// Copyright (c) 1994-2006 Sun Microsystems Inc.
// All Rights Reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//
// - Redistributions of source code must retain the above copyright notice,
// this list of conditions and the following disclaimer.
//
// - Redistribution in binary form must reproduce the above copyright
// notice, this list of conditions and the following disclaimer in the
// documentation and/or other materials provided with the
// distribution.
//
// - Neither the name of Sun Microsystems or the names of contributors may
// be used to endorse or promote products derived from this software without
// specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
// FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
// COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
// STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
// OF THE POSSIBILITY OF SUCH DAMAGE.

// The original source code covered by the above license above has been
// modified significantly by Google Inc.
// Copyright 2014 the V8 project authors. All rights reserved.

// A light-weight SPARC Assembler
// Generates user mode instructions for the SPARC architecture up

#ifndef V8_SPARC_ASSEMBLER_SPARC_H_
#define V8_SPARC_ASSEMBLER_SPARC_H_

#include <stdio.h>
#include <vector>

#include "src/assembler.h"
#include "src/sparc/constants-sparc.h"

namespace v8 {
namespace internal {

// clang-format off
#define GENERAL_REGISTERS(V)                              \
  V(g0)  V(g1)  V(g2)  V(g3)  V(g4)  V(g5)  V(g6)  V(g7)  \
  V(o0)  V(o1)  V(o2)  V(o3)  V(o4)  V(o5)  V(o6)  V(o7)  \
  V(l0)  V(l1)  V(l2)  V(l3)  V(l4)  V(l5)  V(l6)  V(l7)  \
  V(i0)  V(i1)  V(i2)  V(i3)  V(i4)  V(i5)  V(i6)  V(i7) 

#define ALLOCATABLE_GENERAL_REGISTERS(V)                  \
            V(g1)  V(g2)  V(g3)  V(g4)                                   \
  V(o0)  V(o1)  V(o2)  V(o3)  V(o4)  V(o5)                        \
  V(l0)  V(l1)  V(l2)  V(l3)  V(l4)  V(l5)  V(l6)  V(l7)  \
  V(i0)  V(i1)  V(i2)  V(i3)  V(i4)  V(i5)   
    
#define DOUBLE_REGISTERS(V)                               \
  V(f0)  V(f2)  V(f4)  V(f6)  V(f8)  V(f10) V(f12) V(f14) \
  V(f16) V(f18) V(f20) V(f22) V(f24) V(f26) V(f28) V(f30) \
  V(f32) V(f34) V(f36) V(f38) V(f40) V(f42) V(f44) V(f46) \
  V(f48) V(f50) V(f52) V(f54) V(f56) V(f58) V(f60) V(f62) 

#define ALLOCATABLE_DOUBLE_REGISTERS(V)                   \
  V(f0)  V(f2)  V(f4)  V(f6)  V(f8)  V(f10) V(f12) V(f14) \
  V(f16) V(f18) V(f20) V(f22) V(f24) V(f26) V(f28) V(f30) \
  V(f32) V(f34) V(f36) V(f38) V(f40) V(f42) V(f44) V(f46) \
  V(f48) V(f50) V(f52) V(f54) V(f56) V(f58) V(f60) V(f62) 
// clang-format on

// CPU Registers.
//
// 1) We would prefer to use an enum, but enum values are assignment-
// compatible with int, which has caused code-generation bugs.
//
// 2) We would prefer to use a class instead of a struct but we don't like
// the register initialization to depend on the particular initialization
// order (which appears to be different on OS X, Linux, and Windows for the
// installed versions of C++ we tried). Using a struct permits C-style
// "initialization". Also, the Register objects cannot be const as this
// forces initialization stubs in MSVC, making us dependent on initialization
// order.
//
// 3) By not using an enum, we are possibly preventing the compiler from
// doing certain constant folds, which may significantly reduce the
// code generated for some assembly instructions (because they boil down
// to a few constants). If this is a problem, we could change the code
// such that we use an enum in optimized mode, and the struct in debug
// mode. This way we get the compile-time error checking in debug mode
// and best performance in optimized code.

struct Register {
  enum Code {
#define REGISTER_CODE(R) kCode_##R,
    GENERAL_REGISTERS(REGISTER_CODE)
#undef REGISTER_CODE
        kAfterLast,
    kCode_no_reg = -1
  };

  static const int kNumRegisters = Code::kAfterLast;

#define REGISTER_COUNT(R) 1 +
  static const int kNumAllocatable =
      ALLOCATABLE_GENERAL_REGISTERS(REGISTER_COUNT)0;
#undef REGISTER_COUNT

#define REGISTER_BIT(R) 1 << kCode_##R |
  static const RegList kAllocatable =
      ALLOCATABLE_GENERAL_REGISTERS(REGISTER_BIT)0;
#undef REGISTER_BIT

  static Register from_code(int code) {
    DCHECK(code >= 0);
    DCHECK(code < kNumRegisters);
    Register r = {code};
    return r;
  }
  const char* ToString();
  bool IsAllocatable() const;
  bool is_valid() const { return 0 <= reg_code && reg_code < kNumRegisters; }
  bool is(Register reg) const { return reg_code == reg.reg_code; }
  int code() const {
    DCHECK(is_valid());
    return reg_code;
  }
  int bit() const {
    DCHECK(is_valid());
    return 1 << reg_code;
  }
  void set_code(int code) {
    reg_code = code;
    DCHECK(is_valid());
  }

#if V8_TARGET_LITTLE_ENDIAN
  static const int kMantissaOffset = 0;
  static const int kExponentOffset = 4;
#else
  static const int kMantissaOffset = 4;
  static const int kExponentOffset = 0;
#endif

  // Unfortunately we can't make this private in a struct.
  int reg_code;
};

#define DECLARE_REGISTER(R) const Register R = {Register::kCode_##R};
GENERAL_REGISTERS(DECLARE_REGISTER)
#undef DECLARE_REGISTER
const Register no_reg = {Register::kCode_no_reg};


// Coprocessor register.
struct DoubleRegister {
  enum Code {
#define REGISTER_CODE(R) kCode_##R,
    DOUBLE_REGISTERS(REGISTER_CODE)
#undef REGISTER_CODE
        kAfterLast,
    kCode_no_reg = -1
  };

  static const int kMaxNumRegisters = Code::kAfterLast;

  inline static int NumRegisters();

  // TODO(plind): Warning, inconsistent numbering here. kNumFPURegisters refers
  // to number of 32-bit FPU regs, but kNumAllocatableRegisters refers to
  // number of Double regs (64-bit regs, or FPU-reg-pairs).

  const char* ToString();
  bool IsAllocatable() const;
  bool is_valid() const { return 0 <= reg_code && reg_code < kMaxNumRegisters; }
  bool is(DoubleRegister reg) const { return reg_code == reg.reg_code; }
  DoubleRegister low() const {
    // TODO(plind): Create DCHECK for FR=0 mode. This usage suspect for FR=1.
    // Find low reg of a Double-reg pair, which is the reg itself.
    DCHECK(reg_code % 2 == 0);  // Specified Double reg must be even.
    DoubleRegister reg;
    reg.reg_code = reg_code;
    DCHECK(reg.is_valid());
    return reg;
  }
  DoubleRegister high() const {
    // TODO(plind): Create DCHECK for FR=0 mode. This usage illegal in FR=1.
    // Find high reg of a Doubel-reg pair, which is reg + 1.
    DCHECK(reg_code % 2 == 0);  // Specified Double reg must be even.
    DoubleRegister reg;
    reg.reg_code = reg_code + 1;
    DCHECK(reg.is_valid());
    return reg;
  }

  int code() const {
    DCHECK(is_valid());
    return reg_code;
  }
  int bit() const {
    DCHECK(is_valid());
    return 1 << reg_code;
  }

  static DoubleRegister from_code(int code) {
    DoubleRegister r = {code};
    return r;
  }
  void setcode(int f) {
    reg_code = f;
    DCHECK(is_valid());
  }
  // Unfortunately we can't make this private in a struct.
  int reg_code;
};

typedef DoubleRegister FPURegister;
typedef DoubleRegister FloatRegister;

const DoubleRegister no_freg = {-1};

const DoubleRegister f0 = {0};  
const DoubleRegister f2 = {2};
const DoubleRegister f4 = {4};
const DoubleRegister f6 = {6};
const DoubleRegister f8 = {8};
const DoubleRegister f10 = {10};
const DoubleRegister f12 = {12};  
const DoubleRegister f14 = {14};  
const DoubleRegister f16 = {16};
const DoubleRegister f18 = {18};
const DoubleRegister f20 = {20};
const DoubleRegister f22 = {22};
const DoubleRegister f24 = {24};
const DoubleRegister f26 = {26};
const DoubleRegister f28 = {28};
const DoubleRegister f30 = {30};
const DoubleRegister f32 = {32};
const DoubleRegister f34 = {34};
const DoubleRegister f36 = {36};
const DoubleRegister f38 = {38};
const DoubleRegister f40 = {40};
const DoubleRegister f42 = {42};
const DoubleRegister f44 = {44};
const DoubleRegister f46 = {46};
const DoubleRegister f48 = {48};
const DoubleRegister f50 = {50};
const DoubleRegister f52 = {52};
const DoubleRegister f54 = {54};
const DoubleRegister f56 = {56};
const DoubleRegister f58 = {58};
const DoubleRegister f60 = {60};
const DoubleRegister f62 = {62};



// Register aliases.
#define kLithiumScratchReg l0
#define kLithiumScratchReg2 l1
#define kLithiumScratchDouble f60
#define kZeroRegister g0

// Class Operand represents a shifter operand in data processing instructions
class Operand BASE_EMBEDDED {
 public:
   INLINE(explicit Operand(Register rm)) { UNIMPLEMENTED(); }
private:
 
  friend class Assembler;
  friend class MacroAssembler;
};


// Class MemOperand represents a memory operand in load and store instructions
// On PowerPC we have base register + 16bit signed value
// Alternatively we can have a 16bit signed value immediate
class MemOperand BASE_EMBEDDED {
 public:
 private:
 
  friend class Assembler;
};

class Assembler : public AssemblerBase {
public:
  // Create an assembler. Instructions and relocation information are emitted
  // into a buffer, with the instructions starting from the beginning and the
  // relocation information starting from the end of the buffer. See CodeDesc
  // for a detailed comment on the layout (globals.h).
  //
  // If the provided buffer is NULL, the assembler allocates and grows its own
  // buffer, and buffer_size determines the initial buffer size. The buffer is
  // owned by the assembler and deallocated upon destruction of the assembler.
  //
  // If the provided buffer is not NULL, the assembler uses the provided buffer
  // for code generation and assumes its size to be buffer_size. If the buffer
  // is too small, a fatal error occurs. No deallocation of the buffer is done
  // upon destruction of the assembler.
  Assembler(Isolate* isolate, void* buffer, int buffer_size);
  virtual ~Assembler() { }

  // GetCode emits any pending (non-emitted) code and fills the descriptor
  // desc. GetCode() is idempotent; it returns the same result if no other
  // Assembler functions are invoked in between GetCode() calls.
  void GetCode(CodeDesc* desc);

  // Label operations & relative jumps (PPUM Appendix D).
  //
  // Takes a branch opcode (cc) and a label (L) and generates
  // either a backward branch or a forward branch and links it
  // to the label fixup chain. Usage:
  //
  // Label L;    // unbound label
  // j(cc, &L);  // forward branch to unbound label
  // bind(&L);   // bind label to the current pc
  // j(cc, &L);  // backward branch to bound label
  // bind(&L);   // illegal: a label may be bound only once
  //
  // Note: The same Label can be used for forward and backward branches
  // but it may be bound only once.
  void bind(Label* L);  // Binds an unbound label L to current code position.


   // Read/Modify the code target address in the branch/call instruction at pc.
  static Address target_address_at(Address pc) { UNIMPLEMENTED(); }
  static void set_target_address_at(Address pc,
                                    Address target,
                                    ICacheFlushMode icache_flush_mode =
                                        FLUSH_ICACHE_IF_NEEDED) { UNIMPLEMENTED(); }
  INLINE(static Address target_address_at(Address pc, Address constant_pool))  { UNIMPLEMENTED(); }
  INLINE(static void set_target_address_at(
      Address pc, Address constant_pool, Address target,
      ICacheFlushMode icache_flush_mode = FLUSH_ICACHE_IF_NEEDED))  { UNIMPLEMENTED(); }
  INLINE(static Address target_address_at(Address pc, Code* code))  { UNIMPLEMENTED(); }
  INLINE(static void set_target_address_at(Address pc,
                                           Code* code,
                                           Address target,
                                           ICacheFlushMode icache_flush_mode =
                                               FLUSH_ICACHE_IF_NEEDED))  { UNIMPLEMENTED(); }

  // Return the code target address at a call site from the return address
  // of that call in the instruction stream.
  inline static Address target_address_from_return_address(Address pc);

  // This sets the branch destination (which gets loaded at the call address).
  // This is for calls and branches within generated code.  The serializer
  // has already deserialized the lui/ori instructions etc.
  inline static void deserialization_set_special_target_at(
      Address instruction_payload, Code* code, Address target)  { UNIMPLEMENTED(); }

  // This sets the internal reference at the pc.
  inline static void deserialization_set_target_internal_reference_at(
      Address pc, Address target,
      RelocInfo::Mode mode = RelocInfo::INTERNAL_REFERENCE) { UNIMPLEMENTED(); }


  // Difference between address of current opcode and target address offset.
  static const int kBranchPCOffset = 4;

  // Here we are patching the address in the LUI/ORI instruction pair.
  // These values are used in the serialization process and must be zero for
  // MIPS platform, as Code, Embedded Object or External-reference pointers
  // are split across two consecutive instructions and don't exist separately
  // in the code, so the serializer should not step forwards in memory after
  // a target is resolved and written.
  static const int kSpecialTargetSize = 0;

  // Number of consecutive instructions used to store 32bit/64bit constant.
  // This constant was used in RelocInfo::target_address_address() function
  // to tell serializer address of the instruction that follows
  // LUI/ORI instruction pair.
  static const int kInstructionsFor32BitConstant = 2;
  static const int kInstructionsFor64BitConstant = 4;

  // Distance between the instruction referring to the address of the call
  // target and the return address.
  static const int kCallTargetAddressOffset = 6 * kInstructionSize;

  // Distance between start of patched debug break slot and the emitted address
  // to jump to.
  static const int kPatchDebugBreakSlotAddressOffset = 6 * kInstructionSize;

  // Difference between address of current opcode and value read from pc
  // register.
  static const int kPcLoadDelta = 4;

  static const int kDebugBreakSlotInstructions = 6;
  static const int kDebugBreakSlotLength =
      kDebugBreakSlotInstructions * kInstructionSize;


  // Debugging.

  // Mark generator continuation.
  void RecordGeneratorContinuation();

  // Mark address of a debug break slot.
  void RecordDebugBreakSlot(RelocInfo::Mode mode, int argc = 0);

  // Record the AST id of the CallIC being compiled, so that it can be placed
  // in the relocation information.
  void SetRecordedAstId(TypeFeedbackId ast_id) {
    DCHECK(recorded_ast_id_.IsNone());
    recorded_ast_id_ = ast_id;
  }

  TypeFeedbackId RecordedAstId() {
    DCHECK(!recorded_ast_id_.IsNone());
    return recorded_ast_id_;
  }

  void ClearRecordedAstId() { recorded_ast_id_ = TypeFeedbackId::None(); }


  // Record a comment relocation entry that can be used by a disassembler.
  // Use --code-comments to enable.
  void RecordComment(const char* msg);

  // Record a deoptimization reason that can be used by a log or cpu profiler.
  // Use --trace-deopt to enable.
  void RecordDeoptReason(const int reason, const SourcePosition position);

  static int RelocateInternalReference(RelocInfo::Mode rmode, byte* pc,
                                       intptr_t pc_delta);

  // Writes a single byte or word of data in the code stream.  Used for
  // inline tables, e.g., jump-tables.
  void db(uint8_t data);
  void dd(uint32_t data);
  void dq(uint64_t data);
  void dp(uintptr_t data) { dq(data); }
  void dd(Label* label);

  PositionsRecorder* positions_recorder() { return &positions_recorder_; }
  
  void PatchConstantPoolAccessInstruction(int pc_offset, int offset,
                                          ConstantPoolEntry::Access access,
                                          ConstantPoolEntry::Type type) {
     UNIMPLEMENTED();
  }

    // ---------------------------------------------------------------------------
  // Code generation.

  // Insert the smallest number of nop instructions
  // possible to align the pc offset to a multiple
  // of m. m must be a power of 2 (>= 4).
  void Align(int m);
  // Insert the smallest number of zero bytes possible to align the pc offset
  // to a mulitple of m. m must be a power of 2 (>= 2).
  void DataAlign(int m);
  // Aligns code to something that's optimal for a jump target for the platform.
  void CodeTargetAlign();

  void nop() { UNIMPLEMENTED(); }
  
protected:
   // Relocation for a type-recording IC has the AST id added to it.  This
  // member variable is a way to pass the information from the call site to
  // the relocation info.
  TypeFeedbackId recorded_ast_id_;
   
  int64_t buffer_space() const { UNIMPLEMENTED(); }
  
  // Record reloc info for current pc_.
  void RecordRelocInfo(RelocInfo::Mode rmode, intptr_t data = 0);

  
private:
    
    
  // Code generation.
  // The relocation writer's position is at least kGap bytes below the end of
  // the generated instructions. This is so that multi-instruction sequences do
  // not have to check for overflow. The same is true for writes of large
  // relocation info entries.
  // CHECK_NEXT
  static const int kGap = 32;

  void bind_to(Label* L, int pos);
  
   // Code emission
  inline void CheckBuffer();
  void GrowBuffer(int needed = 0);
   
  friend class RelocInfo;
  friend class CodePatcher;
  friend class BlockTrampolinePoolScope;

  PositionsRecorder positions_recorder_;
  friend class PositionsRecorder;
  friend class EnsureSpace;
};


class EnsureSpace BASE_EMBEDDED {
 public:
  explicit EnsureSpace(Assembler* assembler) { assembler->CheckBuffer(); }
};

}  // namespace internal
}  // namespace v8

#endif  // V8_SPARC_ASSEMBLER_SPARC_H_
