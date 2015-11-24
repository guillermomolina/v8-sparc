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

#ifdef DEBUG
#define CHECK_DELAY
#endif

namespace v8 {
namespace internal {

    
// clang-format off
#define GENERAL_REGISTERS(V)                              \
  V(g0)  V(g1)  V(g2)  V(g3)  V(g4)  V(g5)  V(g6)  V(g7)  \
  V(o0)  V(o1)  V(o2)  V(o3)  V(o4)  V(o5)  V(sp)  V(o7)  \
  V(l0)  V(l1)  V(l2)  V(l3)  V(l4)  V(l5)  V(l6)  V(l7)  \
  V(i0)  V(i1)  V(i2)  V(i3)  V(i4)  V(i5)  V(fp)  V(i7) 

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
  bool is_even() const  { return (code() & 1) == 0; }
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

  enum Width {
    S = 1,  D = 2,  Q = 3
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

  int encoding() const { 
      DCHECK(is_valid());
	  return reg_code;
  }

  int encoding(Width w) const {
    const int c = encoding();
    switch (w) {
      case S:
        DCHECK(c < 32); // , "bad single float register");
        return c;

      case D:
        DCHECK(c < 64  &&  (c & 1) == 0); //, "bad double float register");
        return (c & 0x1e) | ((c & 0x20) >> 5);

      case Q:
        DCHECK(c < 64  &&  (c & 3) == 0); //, "bad quad float register");
        return (c & 0x1c) | ((c & 0x20) >> 5);
    }
    UNREACHABLE();
    return -1;
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

#define DECLARE_REGISTER(R) \
  const DoubleRegister R = {DoubleRegister::kCode_##R};
DOUBLE_REGISTERS(DECLARE_REGISTER)
#undef DECLARE_REGISTER
const DoubleRegister no_double_reg = {DoubleRegister::kCode_no_reg};

typedef DoubleRegister FPURegister;
typedef DoubleRegister FloatRegister;

// Register aliases.
#define kLithiumScratchReg l0
#define kLithiumScratchReg2 l1
#define kLithiumScratchDouble f60
#define kZeroRegister g0

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

  // Label operations & relative jumps (PPUM Appendix D)
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

  void bind(Label* L);  // binds an unbound label L to the current code position

  // Links a label at the current pc_offset().  If already bound, returns the
  // bound position.  If already linked, returns the position of the prior link.
  // Otherwise, returns the current pc_offset().
  int link(Label* L);

  // Determines if Label is bound and near enough so that a single
  // branch instruction can be used to reach it.
  bool is_near(Label* L);

  // Returns the branch offset to the given label from the current code position
  // Links the label to the current position if it is still unbound
  int branch_offset(Label* L) {
/*    if (L->is_unused() && !trampoline_emitted_) {
      TrackBranch();
    }*/
    return link(L) - pc_offset();
  }

  // Puts a labels target address at the given position.
  // The high 8 bits are set to zero.
  void label_at_put(Label* L, int at_offset);


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
  
  static const bool UseCBCond = false;

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
                                       int pc_delta);

  // Instruction functions used only for test, debug, and patching.
  // Emit raw instructions in the instruction stream.
  void dci(Instr raw_inst) { Emit(raw_inst); }

  // Emit 8 bits of data in the instruction stream.
  void dc8(uint8_t data) { EmitData(&data, sizeof(data)); }

  // Emit 32 bits of data in the instruction stream.
  void dc32(uint32_t data) { EmitData(&data, sizeof(data)); }

  // Emit 64 bits of data in the instruction stream.
  void dc64(uint64_t data) { EmitData(&data, sizeof(data)); }

  // Emit an address in the instruction stream.
  void dcptr(Label* label);

  // Copy a string into the instruction stream, including the terminating NULL
  // character. The instruction pointer (pc_) is then aligned correctly for
  // subsequent instructions.
  void EmitStringData(const char* string);

  // Pseudo-instructions ------------------------------------------------------

  // Required by V8.
  void dd(uint32_t data) { dc32(data); }
  void db(uint8_t data) { dc8(data); }
  void dq(uint64_t data) { dc64(data); }
  void dp(uintptr_t data) { dc64(data); }


  Instruction* pc() const { return Instruction::Cast(pc_); }

  // Read/patch instructions
  Instr instr_at(int pos) { return *reinterpret_cast<Instr*>(buffer_ + pos); }
  void instr_at_put(int pos, Instr instr) {
    *reinterpret_cast<Instr*>(buffer_ + pos) = instr;
  }
  static Instr instr_at(byte* pc) { return *reinterpret_cast<Instr*>(pc); }
  static void instr_at_put(byte* pc, Instr instr) {
    *reinterpret_cast<Instr*>(pc) = instr;
  }

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
  
protected:
   // Relocation for a type-recording IC has the AST id added to it.  This
  // member variable is a way to pass the information from the call site to
  // the relocation info.
  TypeFeedbackId recorded_ast_id_;
   
  int64_t buffer_space() const { return reloc_info_writer.pos() - pc_; }
  
  // Record reloc info for current pc_.
  void RecordRelocInfo(RelocInfo::Mode rmode, int data = 0);

  // Decode instruction(s) at pos and return backchain to previous
  // label reference or kEndOfChain.
  int target_at(int pos);

  // Patch instruction(s) at pos to target target_pos (e.g. branch)
  void target_at_put(int pos, int target_pos);

  
private:
    
    
  // Code generation.
  // The relocation writer's position is at least kGap bytes below the end of
  // the generated instructions. This is so that multi-instruction sequences do
  // not have to check for overflow. The same is true for writes of large
  // relocation info entries.
  // CHECK_NEXT
  static const int kGap = 32;

    // Relocation information generation.
  // Each relocation is encoded as a variable size value.
  static const int kMaxRelocSize = RelocInfoWriter::kMaxSize;
  RelocInfoWriter reloc_info_writer;

  void bind_to(Label* L, int pos);
  void next(Label* L);
   int last_bound_pos_;
 
   // Code emission
  inline void CheckBuffer();
  void GrowBuffer(int needed = 0);
   
 protected:
  // helpers

  // x is supposed to fit in a field "nbits" wide
  // and be sign-extended. Check the range.

  static void assert_signed_range(int x, int nbits) {
      DCHECK(nbits == 32 || (-(1 << (nbits-1)) <= x  &&  x < ( 1 << (nbits-1))));   // assert value in range
  }

  static void assert_signed_word_disp_range(int x, int nbits) {
      DCHECK((x & 3) == 0); // assert word aligned
      assert_signed_range(x, nbits + 2);
  }

  static void assert_unsigned_const(int x, int nbits) {
      DCHECK( static_cast<uint32_t>(x)  <  static_cast<uint32_t>(1 << nbits)  ); // assert unsigned constant in range
  }

  // fields: note bits numbered from LSB = 0,
  //  fields known by inclusive bit range

  static int fmask(uint32_t hi_bit, uint32_t lo_bit) {
    DCHECK( hi_bit >= lo_bit  &&  0 <= lo_bit  &&  hi_bit < 32); // assert good bits
    return (1 << ( hi_bit-lo_bit + 1 )) - 1;
  }

  // inverse of u_field

  static int inv_u_field(int x, int hi_bit, int lo_bit) {
    uint32_t r = static_cast<uint32_t>(x) >> lo_bit;
    r &= fmask( hi_bit, lo_bit);
    return int(r);
  }


  // signed version: extract from field and sign-extend

  static int inv_s_field(int x, int hi_bit, int lo_bit) {
    int sign_shift = 31 - hi_bit;
    return inv_u_field( ((x << sign_shift) >> sign_shift), hi_bit, lo_bit);
  }

  // given a field that ranges from hi_bit to lo_bit (inclusive,
  // LSB = 0), and an unsigned value for the field,
  // shift it into the field

#ifdef ASSERT
  static int u_field(int x, int hi_bit, int lo_bit) {
    assert( ( x & ~fmask(hi_bit, lo_bit))  == 0,
            "value out of range");
    int r = x << lo_bit;
    assert( inv_u_field(r, hi_bit, lo_bit) == x, "just checking");
    return r;
  }
#else
  // make sure this is inlined as it will reduce code size significantly
  #define u_field(x, hi_bit, lo_bit)   ((x) << (lo_bit))
#endif

  static int inv_op(  int x ) { return inv_u_field(x, 31, 30); }
  static int inv_op2( int x ) { return inv_u_field(x, 24, 22); }
  static int inv_op3( int x ) { return inv_u_field(x, 24, 19); }
  static int inv_cond( int x ){ return inv_u_field(x, 28, 25); }

  static bool inv_immed( int x ) { return (x & Assembler::immed(true)) != 0; }

  static Register inv_rd(  int x ) { return Register::from_code(inv_u_field(x, 29, 25)); }
  static Register inv_rs1( int x ) { return Register::from_code(inv_u_field(x, 18, 14)); }
  static Register inv_rs2( int x ) { return Register::from_code(inv_u_field(x,  4,  0)); }

  static int op(       int         x)  { return  u_field(x,             31, 30); }
  static int rd(       Register    r)  { return  u_field(r.code(), 29, 25); }
  static int fcn(      int         x)  { return  u_field(x,             29, 25); }
  static int op3(      int         x)  { return  u_field(x,             24, 19); }
  static int rs1(      Register    r)  { return  u_field(r.code(), 18, 14); }
  static int rs2(      Register    r)  { return  u_field(r.code(),  4,  0); }
  static int annul(    bool        a)  { return  u_field(a ? 1 : 0,     29, 29); }
  static int cond(     int         x)  { return  u_field(x,             28, 25); }
  static int cond_mov( int         x)  { return  u_field(x,             17, 14); }
  static int rcond(    RCondition  x)  { return  u_field(x,             12, 10); }
  static int op2(      int         x)  { return  u_field(x,             24, 22); }
  static int predict(  bool        p)  { return  u_field(p ? 1 : 0,     19, 19); }
  static int branchcc( CC       fcca)  { return  u_field(fcca,          21, 20); }
  static int cmpcc(    CC       fcca)  { return  u_field(fcca,          26, 25); }
  static int imm_asi(  int         x)  { return  u_field(x,             12,  5); }
  static int immed(    bool        i)  { return  u_field(i ? 1 : 0,     13, 13); }
  static int opf_low6( int         w)  { return  u_field(w,             10,  5); }
  static int opf_low5( int         w)  { return  u_field(w,              9,  5); }
  static int op5(      int         x)  { return  u_field(x,              8,  5); }
  static int trapcc(   CC         cc)  { return  u_field(cc,            12, 11); }
  static int sx(       int         i)  { return  u_field(i,             12, 12); } // shift x=1 means 64-bit
  static int opf(      int         x)  { return  u_field(x,             13,  5); }

  static bool is_cbcond( int x ) {
    return (/*VM_Version::has_cbcond() */false && (inv_cond(x) > rc_last) &&
            inv_op(x) == branch_op && inv_op2(x) == bpr_op2);
  }
  static bool is_cxb( int x ) {
    DCHECK(is_cbcond(x)); // assert instruction
    return (x & (1<<21)) != 0;
  }
  static int cond_cbcond( int         x)  { return  u_field((((x & 8)<<1) + 8 + (x & 7)), 29, 25); }
  static int inv_cond_cbcond(int      x)  {
    DCHECK(is_cbcond(x)); // assert instruction
    return inv_u_field(x, 27, 25) | (inv_u_field(x, 29, 29)<<3);
  }

  static int opf_cc(   CC          c, bool useFloat ) { return u_field((useFloat ? 0 : 4) + c, 13, 11); }
  static int mov_cc(   CC          c, bool useFloat ) { return u_field(useFloat ? 0 : 1,  18, 18) | u_field(c, 12, 11); }

  static int fd( FloatRegister r,  FloatRegister::Width fwa) { return u_field(r.encoding(fwa), 29, 25); };
  static int fs1(FloatRegister r,  FloatRegister::Width fwa) { return u_field(r.encoding(fwa), 18, 14); };
  static int fs2(FloatRegister r,  FloatRegister::Width fwa) { return u_field(r.encoding(fwa),  4,  0); };
  static int fs3(FloatRegister r,  FloatRegister::Width fwa) { return u_field(r.encoding(fwa),  13,  9); };

  // some float instructions use this encoding on the op3 field
  static int alt_op3(int op, FloatRegister::Width w) {
    int r;
    switch(w) {
     case FloatRegister::S: r = op + 0;  break;
     case FloatRegister::D: r = op + 3;  break;
     case FloatRegister::Q: r = op + 2;  break;
     default: UNREACHABLE(); break;
    }
    return op3(r);
  }

  // min and max values for signed immediate ranges
  static int min_simm(int nbits) { return -(static_cast<int>(1) << (nbits - 1))    ; }
  static int max_simm(int nbits) { return  (static_cast<int>(1) << (nbits - 1)) - 1; }

  // Define some:
  static int min_simm10() { return min_simm(10); }
  static int min_simm13() { return min_simm(13); }
  static int min_simm16() { return min_simm(16); }

  // Test if x is within signed immediate range for nbits
  static bool is_simm(int x, int nbits) { return min_simm(nbits) <= x && x <= max_simm(nbits); }

  // Define some:
  static bool is_simm5( int x) { return is_simm(x, 5 ); }
  static bool is_simm8( int x) { return is_simm(x, 8 ); }
  static bool is_simm10(int x) { return is_simm(x, 10); }
  static bool is_simm11(int x) { return is_simm(x, 11); }
  static bool is_simm12(int x) { return is_simm(x, 12); }
  static bool is_simm13(int x) { return is_simm(x, 13); }
  static bool is_simm16(int x) { return is_simm(x, 16); }
  static bool is_simm26(int x) { return is_simm(x, 26); }
  static bool is_simm32(int x) { return is_simm(x, 32); }

  static bool is_in_wdisp_range(int a, int b, int nbits) {
    int d = int(b) - int(a);
    return is_simm(d, nbits + 2);
  }

/*
  // test if label is in simm16 range in words (wdisp16).
  bool is_in_wdisp16_range(Label* L) {
    return is_in_wdisp_range(target_distance(L), pc(), 16);
  }
  // test if the distance between two addresses fits in simm30 range in words
  static bool is_in_wdisp30_range(int a, int b) {
    return is_in_wdisp_range(a, b, 30);
  }
*/

  // compute inverse of simm
  static int inv_simm(int x, int nbits) {
    return static_cast<int>(x << (32 - nbits)) >> (32 - nbits);
  }

  static int inv_simm13( int x ) { return inv_simm(x, 13); }

  // signed immediate, in low bits, nbits long
  static int simm(int x, int nbits) {
    assert_signed_range(x, nbits);
    return x  &  (( 1 << nbits ) - 1);
  }


  // compute inverse of wdisp16
  static int inv_wdisp16(int x) {
    int lo = x & (( 1 << 14 ) - 1);
    int hi = (x >> 20) & 3;
    if (hi >= 2) hi |= ~1;
    return (((hi << 14) | lo) << 2);
  }

  // word offset, 14 bits at LSend, 2 bits at B21, B20
  static int wdisp16( int offset) {
    assert_signed_word_disp_range(offset, 16);
    int r =  ((offset >> 2) & ((1 << 14) - 1))
           |  (  ( (offset>>(2+14)) & 3 )  <<  20 );
    DCHECK( inv_wdisp16(r) == offset); //,  "inverse is not inverse");
    return r;
  }

  // compute inverse of wdisp10
  static int inv_wdisp10(int x) {
    DCHECK(is_cbcond(x));//, "wrong instruction");
    int lo = inv_u_field(x, 12, 5);
    int hi = (x >> 19) & 3;
    if (hi >= 2) hi |= ~1;
    return (((hi << 8) | lo) << 2);
  }

  // word offset for cbcond, 8 bits at [B12,B5], 2 bits at [B20,B19]
  static int wdisp10(int offset) {
   /* assert(VM_Version::has_cbcond(), "This CPU does not have CBCOND instruction");*/
    DCHECK(false); //Do no use cbond
    assert_signed_word_disp_range(offset, 10);
    int r =  ( ( (offset >>  2   ) & ((1 << 8) - 1) ) <<  5 )
           | ( ( (offset >> (2+8)) & 3              ) << 19 );
    // Have to fake cbcond instruction to pass assert in inv_wdisp10()
    DCHECK(inv_wdisp10((r | op(branch_op) | cond_cbcond(rc_last+1) | op2(bpr_op2))) == offset);//,  "inverse is not inverse");
    return r;
  }

  // word displacement in low-order nbits bits

  static int inv_wdisp( int x, int nbits ) {
    int pre_sign_extend = x & (( 1 << nbits ) - 1);
    int r =  pre_sign_extend >= ( 1 << (nbits-1) )
       ?   pre_sign_extend | ~(( 1 << nbits ) - 1)
       :   pre_sign_extend;
    return (r << 2);
  }

  static int wdisp( int offset,int nbits ) {
    assert_signed_word_disp_range(offset, nbits);
    int r =  (offset >> 2) & (( 1 << nbits ) - 1);
    DCHECK( inv_wdisp( r, nbits )  ==  offset);//, "inverse not inverse");
    return r;
  }


  // Extract the top 32 bits in a 64 bit word
  static int32_t hi32( int64_t x ) {
    int32_t r = int32_t( (uint64_t)x >> 32 );
    return r;
  }

  // given a sethi instruction, extract the constant, left-justified
  static int inv_hi22( int x ) {
    return x << 10;
  }

  // create an imm22 field, given a 32-bit left-justified constant
  static int hi22( int x ) {
    int r = int( uint32_t(x) >> 10 );
    DCHECK( (r & ~((1 << 22) - 1))  ==  0);//, "just checkin'");
    return r;
  }

  // create a low10 __value__ (not a field) for a given a 32-bit constant
  static int low10( int x ) {
    return x & ((1 << 10) - 1);
  }

  // create a low12 __value__ (not a field) for a given a 32-bit constant
  static int low12( int x ) {
    return x & ((1 << 12) - 1);
  }

  // AES crypto instructions supported only on certain processors
  static void aes_only() { DCHECK(false);/*assert( VM_Version::has_aes(), "This instruction only works on SPARC with AES instructions support");*/ }

  // SHA crypto instructions supported only on certain processors
  static void sha1_only()   { DCHECK(false);/*assert( VM_Version::has_sha1(),   "This instruction only works on SPARC with SHA1"); */}
  static void sha256_only() { DCHECK(false);/*assert( VM_Version::has_sha256(), "This instruction only works on SPARC with SHA256"); */}
  static void sha512_only() { DCHECK(false);/*assert( VM_Version::has_sha512(), "This instruction only works on SPARC with SHA512"); */}

  // CRC32C instruction supported only on certain processors
  static void crc32c_only() { DCHECK(false);/*assert( VM_Version::has_crc32c(), "This instruction only works on SPARC with CRC32C"); */}

  // instruction only in VIS1
  static void vis1_only() { DCHECK(false);/*assert( VM_Version::has_vis1(), "This instruction only works on SPARC with VIS1");*/ }

  // instruction only in VIS2
  static void vis2_only() { DCHECK(false);/*assert( VM_Version::has_vis2(), "This instruction only works on SPARC with VIS2"); */}

  // instruction only in VIS3
  static void vis3_only() {DCHECK(false);/* assert( VM_Version::has_vis3(), "This instruction only works on SPARC with VIS3");*/ }

  // instruction only in v9
  static void v9_only() { } // do nothing

  // instruction deprecated in v9
  static void v9_dep()  { DCHECK(false); } 

  // v8 has no CC field
  static void v8_no_cc(CC cc)  { if (cc)  v9_only(); }

 protected:
  // Simple delay-slot scheme:
  // In order to check the programmer, the assembler keeps track of deley slots.
  // It forbids CTIs in delay slots (conservative, but should be OK).
  // Also, when putting an instruction into a delay slot, you must say
  // asm->delayed()->add(...), in order to check that you don't omit
  // delay-slot instructions.
  // To implement this, we use a simple FSA

#ifdef CHECK_DELAY
  enum Delay_state { no_delay, at_delay_slot, filling_delay_slot } delay_state;
#endif

 public:
  // Tells assembler next instruction must NOT be in delay slot.
  // Use at start of multinstruction macros.
   void assert_not_delayed() {
#ifdef CHECK_DELAY
    DCHECK(delay_state == no_delay);//, msg);
#endif
  }

 protected:
  // Insert a nop if the previous is cbcond
  void insert_nop_after_cbcond() {
    if (UseCBCond && cbcond_before()) {
      nop();
    }
  }
  // Delay slot helpers
  // cti is called when emitting control-transfer instruction,
  // BEFORE doing the emitting.
  // Only effective when assertion-checking is enabled.
  void cti() {
    // A cbcond instruction immediately followed by a CTI
    // instruction introduces pipeline stalls, we need to avoid that.
    no_cbcond_before();
#ifdef CHECK_DELAY
    assert_not_delayed();  //"cti should not be in delay slot"
#endif
  }

  // called when emitting cti with a delay slot, AFTER emitting
  void has_delay_slot() {
#ifdef CHECK_DELAY
    assert_not_delayed();
    delay_state = at_delay_slot;
#endif
  }

  // cbcond instruction should not be generated one after an other
  bool cbcond_before() {
    if (pc_offset() == 0) return false; // it is first instruction
    int x = *(pc_  - kInstructionSize); // previous instruction
    return is_cbcond(x);
  }

  void no_cbcond_before() {
    DCHECK(pc_offset() == 0 || !cbcond_before());//, "cbcond should not follow an other cbcond");
  }

public:

  bool use_cbcond(Label* L) {
    if (!UseCBCond || cbcond_before()) return false;
    int x = branch_offset(L);;
    DCHECK( (x & 3) == 0);//, "not word aligned");
    return is_simm12(x);
  }

  // Tells assembler you know that next instruction is delayed
  Assembler* delayed() {
#ifdef CHECK_DELAY
    DCHECK( delay_state == at_delay_slot); //, "delayed instruction is not in delay slot");
    delay_state = filling_delay_slot;
#endif
    return this;
  }
/*
  void flush() {
#ifdef CHECK_DELAY
    assert ( delay_state == no_delay, "ending code with a delay slot");
#endif
    AssemblerBase::flush();
  }
*/

  // helper for above fcns
  inline void check_delay();


 public:
  // instructions, refer to page numbers in the SPARC Architecture Manual, V9

  // pp 135 (addc was addx in v8)

  inline void add(Register s1, Register s2, Register d );
  inline void add(Register s1, int simm13a, Register d );

  void addcc(  Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(add_op3  | cc_bit_op3) | rs1(s1) | rs2(s2) ); }
  void addcc(  Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(add_op3  | cc_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void addc(   Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(addc_op3             ) | rs1(s1) | rs2(s2) ); }
  void addc(   Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(addc_op3             ) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void addccc( Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(addc_op3 | cc_bit_op3) | rs1(s1) | rs2(s2) ); }
  void addccc( Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(addc_op3 | cc_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }


  // 4-operand AES instructions

  void aes_eround01(  FloatRegister s1, FloatRegister s2, FloatRegister s3, FloatRegister d ) { aes_only(); Emit( op(arith_op) | fd(d, FloatRegister::D) | op3(aes4_op3) | fs1(s1, FloatRegister::D) | fs3(s3, FloatRegister::D) | op5(aes_eround01_op5) | fs2(s2, FloatRegister::D) ); }
  void aes_eround23(  FloatRegister s1, FloatRegister s2, FloatRegister s3, FloatRegister d ) { aes_only(); Emit( op(arith_op) | fd(d, FloatRegister::D) | op3(aes4_op3) | fs1(s1, FloatRegister::D) | fs3(s3, FloatRegister::D) | op5(aes_eround23_op5) | fs2(s2, FloatRegister::D) ); }
  void aes_dround01(  FloatRegister s1, FloatRegister s2, FloatRegister s3, FloatRegister d ) { aes_only(); Emit( op(arith_op) | fd(d, FloatRegister::D) | op3(aes4_op3) | fs1(s1, FloatRegister::D) | fs3(s3, FloatRegister::D) | op5(aes_dround01_op5) | fs2(s2, FloatRegister::D) ); }
  void aes_dround23(  FloatRegister s1, FloatRegister s2, FloatRegister s3, FloatRegister d ) { aes_only(); Emit( op(arith_op) | fd(d, FloatRegister::D) | op3(aes4_op3) | fs1(s1, FloatRegister::D) | fs3(s3, FloatRegister::D) | op5(aes_dround23_op5) | fs2(s2, FloatRegister::D) ); }
  void aes_eround01_l(  FloatRegister s1, FloatRegister s2, FloatRegister s3, FloatRegister d ) { aes_only(); Emit( op(arith_op) | fd(d, FloatRegister::D) | op3(aes4_op3) | fs1(s1, FloatRegister::D) | fs3(s3, FloatRegister::D) | op5(aes_eround01_l_op5) | fs2(s2, FloatRegister::D) ); }
  void aes_eround23_l(  FloatRegister s1, FloatRegister s2, FloatRegister s3, FloatRegister d ) { aes_only(); Emit( op(arith_op) | fd(d, FloatRegister::D) | op3(aes4_op3) | fs1(s1, FloatRegister::D) | fs3(s3, FloatRegister::D) | op5(aes_eround23_l_op5) | fs2(s2, FloatRegister::D) ); }
  void aes_dround01_l(  FloatRegister s1, FloatRegister s2, FloatRegister s3, FloatRegister d ) { aes_only(); Emit( op(arith_op) | fd(d, FloatRegister::D) | op3(aes4_op3) | fs1(s1, FloatRegister::D) | fs3(s3, FloatRegister::D) | op5(aes_dround01_l_op5) | fs2(s2, FloatRegister::D) ); }
  void aes_dround23_l(  FloatRegister s1, FloatRegister s2, FloatRegister s3, FloatRegister d ) { aes_only(); Emit( op(arith_op) | fd(d, FloatRegister::D) | op3(aes4_op3) | fs1(s1, FloatRegister::D) | fs3(s3, FloatRegister::D) | op5(aes_dround23_l_op5) | fs2(s2, FloatRegister::D) ); }
  void aes_kexpand1(  FloatRegister s1, FloatRegister s2, int imm5a, FloatRegister d ) { aes_only(); Emit( op(arith_op) | fd(d, FloatRegister::D) | op3(aes4_op3) | fs1(s1, FloatRegister::D) | u_field(imm5a, 13, 9) | op5(aes_kexpand1_op5) | fs2(s2, FloatRegister::D) ); }


  // 3-operand AES instructions

  void aes_kexpand0(  FloatRegister s1, FloatRegister s2, FloatRegister d ) { aes_only(); Emit( op(arith_op) | fd(d, FloatRegister::D) | op3(aes3_op3) | fs1(s1, FloatRegister::D) | opf(aes_kexpand0_opf) | fs2(s2, FloatRegister::D) ); }
  void aes_kexpand2(  FloatRegister s1, FloatRegister s2, FloatRegister d ) { aes_only(); Emit( op(arith_op) | fd(d, FloatRegister::D) | op3(aes3_op3) | fs1(s1, FloatRegister::D) | opf(aes_kexpand2_opf) | fs2(s2, FloatRegister::D) ); }

  // pp 136

  inline void bpr(RCondition c, bool a, Predict p, Register s1, int disp16);
  inline void bpr(RCondition c, bool a, Predict p, Register s1, Label* L);

  // compare and branch
  inline void cbcond(Condition c, CC cc, Register s1, Register s2, Label* L);
  inline void cbcond(Condition c, CC cc, Register s1, int simm5, Label* L);

  // pp 138

  inline void fb( FPUCondition c, bool a, int disp22 );
  inline void fb( FPUCondition c, bool a, Label* L );

  // pp 141

  inline void fbp( FPUCondition c, bool a, CC cc, Predict p, int disp19 );
  inline void fbp( FPUCondition c, bool a, CC cc, Predict p, Label* L );

  // pp 144

  inline void br( Condition c, bool a, int disp22 );
  inline void br( Condition c, bool a, Label* L );

  // pp 146

  inline void bp( Condition c, bool a, CC cc, Predict p, int disp19 );
  inline void bp( Condition c, bool a, CC cc, Predict p, Label* L );

  // pp 149

  inline void call(int disp30 );
  inline void call( Label* L );

  // pp 150

  // These instructions compare the contents of s2 with the contents of
  // memory at address in s1. If the values are equal, the contents of memory
  // at address s1 is swapped with the data in d. If the values are not equal,
  // the the contents of memory at s1 is loaded into d, without the swap.

  void casa(  Register s1, Register s2, Register d, int ia = -1 ) { v9_only();  Emit( op(ldst_op) | rd(d) | op3(casa_op3 ) | rs1(s1) | (ia == -1  ? immed(true) : imm_asi(ia)) | rs2(s2)); }
  void casxa( Register s1, Register s2, Register d, int ia = -1 ) { v9_only();  Emit( op(ldst_op) | rd(d) | op3(casxa_op3) | rs1(s1) | (ia == -1  ? immed(true) : imm_asi(ia)) | rs2(s2)); }

  // pp 152

  void udiv(   Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(udiv_op3             ) | rs1(s1) | rs2(s2)); }
  void udiv(   Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(udiv_op3             ) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void sdiv(   Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(sdiv_op3             ) | rs1(s1) | rs2(s2)); }
  void sdiv(   Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(sdiv_op3             ) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void udivcc( Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(udiv_op3 | cc_bit_op3) | rs1(s1) | rs2(s2)); }
  void udivcc( Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(udiv_op3 | cc_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void sdivcc( Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(sdiv_op3 | cc_bit_op3) | rs1(s1) | rs2(s2)); }
  void sdivcc( Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(sdiv_op3 | cc_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }

  // pp 155

  void done()  { v9_only();  cti();  Emit( op(arith_op) | fcn(0) | op3(done_op3) ); }
  void retry() { v9_only();  cti();  Emit( op(arith_op) | fcn(1) | op3(retry_op3) ); }

  // pp 156

  void fadd( FloatRegister::Width w, FloatRegister s1, FloatRegister s2, FloatRegister d ) { Emit( op(arith_op) | fd(d, w) | op3(fpop1_op3) | fs1(s1, w) | opf(0x40 + w) | fs2(s2, w)); }
  void fsub( FloatRegister::Width w, FloatRegister s1, FloatRegister s2, FloatRegister d ) { Emit( op(arith_op) | fd(d, w) | op3(fpop1_op3) | fs1(s1, w) | opf(0x44 + w) | fs2(s2, w)); }
 

  // pp 157

  void fcmp(  FloatRegister::Width w, CC cc, FloatRegister s1, FloatRegister s2) { Emit( op(arith_op) | cmpcc(cc) | op3(fpop2_op3) | fs1(s1, w) | opf(0x50 + w) | fs2(s2, w)); }
  void fcmpe( FloatRegister::Width w, CC cc, FloatRegister s1, FloatRegister s2) { Emit( op(arith_op) | cmpcc(cc) | op3(fpop2_op3) | fs1(s1, w) | opf(0x54 + w) | fs2(s2, w)); }

  // pp 159

  void ftox( FloatRegister::Width w, FloatRegister s, FloatRegister d ) { v9_only();  Emit( op(arith_op) | fd(d, FloatRegister::D) | op3(fpop1_op3) | opf(0x80 + w) | fs2(s, w)); }
  void ftoi( FloatRegister::Width w, FloatRegister s, FloatRegister d ) {  Emit( op(arith_op) | fd(d, FloatRegister::S) | op3(fpop1_op3) | opf(0xd0 + w) | fs2(s, w)); }

  // pp 160

  void ftof( FloatRegister::Width sw, FloatRegister::Width dw, FloatRegister s, FloatRegister d ) { Emit( op(arith_op) | fd(d, dw) | op3(fpop1_op3) | opf(0xc0 + sw + dw*4) | fs2(s, sw)); }

  // pp 161

  void fxtof( FloatRegister::Width w, FloatRegister s, FloatRegister d ) { v9_only();  Emit( op(arith_op) | fd(d, w) | op3(fpop1_op3) | opf(0x80 + w*4) | fs2(s, FloatRegister::D)); }
  void fitof( FloatRegister::Width w, FloatRegister s, FloatRegister d ) {             Emit( op(arith_op) | fd(d, w) | op3(fpop1_op3) | opf(0xc0 + w*4) | fs2(s, FloatRegister::S)); }

  // pp 162

  void fmov( FloatRegister::Width w, FloatRegister s, FloatRegister d ) { Emit( op(arith_op) | fd(d, w) | op3(fpop1_op3) | opf(0x00 + w) | fs2(s, w)); }

  void fneg( FloatRegister::Width w, FloatRegister s, FloatRegister d ) { Emit( op(arith_op) | fd(d, w) | op3(fpop1_op3) | opf(0x04 + w) | fs2(s, w)); }

  void fabs( FloatRegister::Width w, FloatRegister s, FloatRegister d ) { Emit( op(arith_op) | fd(d, w) | op3(fpop1_op3) | opf(0x08 + w) | fs2(s, w)); }

  // pp 163

  void fmul( FloatRegister::Width w, FloatRegister s1, FloatRegister s2, FloatRegister d ) { Emit( op(arith_op) | fd(d, w)  | op3(fpop1_op3) | fs1(s1, w)  | opf(0x48 + w)         | fs2(s2, w)); }
  void fmul( FloatRegister::Width sw, FloatRegister::Width dw,  FloatRegister s1, FloatRegister s2, FloatRegister d ) { Emit( op(arith_op) | fd(d, dw) | op3(fpop1_op3) | fs1(s1, sw) | opf(0x60 + sw + dw*4) | fs2(s2, sw)); }
  void fdiv( FloatRegister::Width w, FloatRegister s1, FloatRegister s2, FloatRegister d ) { Emit( op(arith_op) | fd(d, w)  | op3(fpop1_op3) | fs1(s1, w)  | opf(0x4c + w)         | fs2(s2, w)); }

  // FXORs/FXORd instructions

  void fxor( FloatRegister::Width w, FloatRegister s1, FloatRegister s2, FloatRegister d ) { vis1_only(); Emit( op(arith_op) | fd(d, w) | op3(flog3_op3) | fs1(s1, w) | opf(0x6E - w) | fs2(s2, w)); }

  // pp 164

  void fsqrt( FloatRegister::Width w, FloatRegister s, FloatRegister d ) { Emit( op(arith_op) | fd(d, w) | op3(fpop1_op3) | opf(0x28 + w) | fs2(s, w)); }

  // pp 165

  inline void flush( Register s1, Register s2 );
  inline void flush( Register s1, int simm13a);

  // pp 167

  void flushw() { v9_only();  Emit( op(arith_op) | op3(flushw_op3) ); }

  // pp 168

  void illtrap( int const22a) { if (const22a != 0) v9_only();  Emit( op(branch_op) | u_field(const22a, 21, 0) ); }
  // v8 unimp == illtrap(0)

  // pp 169

  void impdep1( int id1, int const19a ) { v9_only();  Emit( op(arith_op) | fcn(id1) | op3(impdep1_op3) | u_field(const19a, 18, 0)); }
  void impdep2( int id1, int const19a ) { v9_only();  Emit( op(arith_op) | fcn(id1) | op3(impdep2_op3) | u_field(const19a, 18, 0)); }

  // pp 170

  void jmpl( Register s1, Register s2, Register d );
  void jmpl( Register s1, int simm13a, Register d );

  // 171

  inline void ldf(FloatRegister::Width w, Register s1, Register s2, FloatRegister d);
  inline void ldf(FloatRegister::Width w, Register s1, int simm13a, FloatRegister d);


  inline void ldfsr(  Register s1, Register s2 );
  inline void ldfsr(  Register s1, int simm13a);
  inline void ldxfsr( Register s1, Register s2 );
  inline void ldxfsr( Register s1, int simm13a);

  // 173

  void ldfa( FloatRegister::Width w, Register s1, Register s2, int ia, FloatRegister d ) { v9_only();  Emit( op(ldst_op) | fd(d, w) | alt_op3(ldf_op3 | alt_bit_op3, w) | rs1(s1) | imm_asi(ia) | rs2(s2) ); }
  void ldfa( FloatRegister::Width w, Register s1, int simm13a, FloatRegister d ) { v9_only();  Emit( op(ldst_op) | fd(d, w) | alt_op3(ldf_op3 | alt_bit_op3, w) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }

  // pp 175, lduw is ld on v8

  inline void ldsb(  Register s1, Register s2, Register d );
  inline void ldsb(  Register s1, int simm13a, Register d);
  inline void ldsh(  Register s1, Register s2, Register d );
  inline void ldsh(  Register s1, int simm13a, Register d);
  inline void ldsw(  Register s1, Register s2, Register d );
  inline void ldsw(  Register s1, int simm13a, Register d);
  inline void ldub(  Register s1, Register s2, Register d );
  inline void ldub(  Register s1, int simm13a, Register d);
  inline void lduh(  Register s1, Register s2, Register d );
  inline void lduh(  Register s1, int simm13a, Register d);
  inline void lduw(  Register s1, Register s2, Register d );
  inline void lduw(  Register s1, int simm13a, Register d);
  inline void ldx(   Register s1, Register s2, Register d );
  inline void ldx(   Register s1, int simm13a, Register d);
  inline void ldd(   Register s1, Register s2, Register d );
  inline void ldd(   Register s1, int simm13a, Register d);

  // pp 177

  void ldsba(  Register s1, Register s2, int ia, Register d ) {             Emit( op(ldst_op) | rd(d) | op3(ldsb_op3 | alt_bit_op3) | rs1(s1) | imm_asi(ia) | rs2(s2) ); }
  void ldsba(  Register s1, int simm13a,         Register d ) {             Emit( op(ldst_op) | rd(d) | op3(ldsb_op3 | alt_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void ldsha(  Register s1, Register s2, int ia, Register d ) {             Emit( op(ldst_op) | rd(d) | op3(ldsh_op3 | alt_bit_op3) | rs1(s1) | imm_asi(ia) | rs2(s2) ); }
  void ldsha(  Register s1, int simm13a,         Register d ) {             Emit( op(ldst_op) | rd(d) | op3(ldsh_op3 | alt_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void ldswa(  Register s1, Register s2, int ia, Register d ) { v9_only();  Emit( op(ldst_op) | rd(d) | op3(ldsw_op3 | alt_bit_op3) | rs1(s1) | imm_asi(ia) | rs2(s2) ); }
  void ldswa(  Register s1, int simm13a,         Register d ) { v9_only();  Emit( op(ldst_op) | rd(d) | op3(ldsw_op3 | alt_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void lduba(  Register s1, Register s2, int ia, Register d ) {             Emit( op(ldst_op) | rd(d) | op3(ldub_op3 | alt_bit_op3) | rs1(s1) | imm_asi(ia) | rs2(s2) ); }
  void lduba(  Register s1, int simm13a,         Register d ) {             Emit( op(ldst_op) | rd(d) | op3(ldub_op3 | alt_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void lduha(  Register s1, Register s2, int ia, Register d ) {             Emit( op(ldst_op) | rd(d) | op3(lduh_op3 | alt_bit_op3) | rs1(s1) | imm_asi(ia) | rs2(s2) ); }
  void lduha(  Register s1, int simm13a,         Register d ) {             Emit( op(ldst_op) | rd(d) | op3(lduh_op3 | alt_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void lduwa(  Register s1, Register s2, int ia, Register d ) {             Emit( op(ldst_op) | rd(d) | op3(lduw_op3 | alt_bit_op3) | rs1(s1) | imm_asi(ia) | rs2(s2) ); }
  void lduwa(  Register s1, int simm13a,         Register d ) {             Emit( op(ldst_op) | rd(d) | op3(lduw_op3 | alt_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void ldxa(   Register s1, Register s2, int ia, Register d ) { v9_only();  Emit( op(ldst_op) | rd(d) | op3(ldx_op3  | alt_bit_op3) | rs1(s1) | imm_asi(ia) | rs2(s2) ); }
  void ldxa(   Register s1, int simm13a,         Register d ) { v9_only();  Emit( op(ldst_op) | rd(d) | op3(ldx_op3  | alt_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }

  // pp 181

  void and3(    Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(and_op3              ) | rs1(s1) | rs2(s2) ); }
  void and3(    Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(and_op3              ) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void andcc(   Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(and_op3  | cc_bit_op3) | rs1(s1) | rs2(s2) ); }
  void andcc(   Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(and_op3  | cc_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void andn(    Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(andn_op3             ) | rs1(s1) | rs2(s2) ); }
  void andn(    Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(andn_op3             ) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void andncc(  Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(andn_op3 | cc_bit_op3) | rs1(s1) | rs2(s2) ); }
  void andncc(  Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(andn_op3 | cc_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void or3(     Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(or_op3               ) | rs1(s1) | rs2(s2) ); }
  void or3(     Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(or_op3               ) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void orcc(    Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(or_op3   | cc_bit_op3) | rs1(s1) | rs2(s2) ); }
  void orcc(    Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(or_op3   | cc_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void orn(     Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(orn_op3) | rs1(s1) | rs2(s2) ); }
  void orn(     Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(orn_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void orncc(   Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(orn_op3  | cc_bit_op3) | rs1(s1) | rs2(s2) ); }
  void orncc(   Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(orn_op3  | cc_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void xor3(    Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(xor_op3              ) | rs1(s1) | rs2(s2) ); }
  void xor3(    Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(xor_op3              ) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void xorcc(   Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(xor_op3  | cc_bit_op3) | rs1(s1) | rs2(s2) ); }
  void xorcc(   Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(xor_op3  | cc_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void xnor(    Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(xnor_op3             ) | rs1(s1) | rs2(s2) ); }
  void xnor(    Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(xnor_op3             ) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void xnorcc(  Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(xnor_op3 | cc_bit_op3) | rs1(s1) | rs2(s2) ); }
  void xnorcc(  Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(xnor_op3 | cc_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }

  // pp 183

  void membar( Membar_mask_bits const7a ) { v9_only(); Emit( op(arith_op) | op3(membar_op3) | rs1(o7) | immed(true) | u_field( int(const7a), 6, 0)); }

  // pp 185

  void fmov( FloatRegister::Width w, FPUCondition c,  bool floatCC, CC cca, FloatRegister s2, FloatRegister d ) { v9_only();  Emit( op(arith_op) | fd(d, w) | op3(fpop2_op3) | cond_mov(c) | opf_cc(cca, floatCC) | opf_low6(w) | fs2(s2, w)); }
  
  // pp 189

  void fmov( FloatRegister::Width w, RCondition c, Register s1,  FloatRegister s2, FloatRegister d ) { v9_only();  Emit( op(arith_op) | fd(d, w) | op3(fpop2_op3) | rs1(s1) | rcond(c) | opf_low5(4 + w) | fs2(s2, w)); }

  // pp 191

  void movcc( Condition c, CC cca, Register s2, Register d ) { v9_only();  Emit( op(arith_op) | rd(d) | op3(movcc_op3) | mov_cc(cca, false) | cond_mov(c) | rs2(s2) ); }
  void movcc( Condition c, CC cca, int simm11a, Register d ) { v9_only();  Emit( op(arith_op) | rd(d) | op3(movcc_op3) | mov_cc(cca, false) | cond_mov(c) | immed(true) | simm(simm11a, 11) ); }
  void movfcc( FPUCondition c, CC cca, Register s2, Register d ) { v9_only();  Emit( op(arith_op) | rd(d) | op3(movcc_op3) | mov_cc(cca, true) | cond_mov(c) | rs2(s2) ); }
  void movfcc( FPUCondition c, CC cca, int simm11a, Register d ) { v9_only();  Emit( op(arith_op) | rd(d) | op3(movcc_op3) | mov_cc(cca, true) | cond_mov(c) | immed(true) | simm(simm11a, 11) ); }

  // pp 195

  void movr( RCondition c, Register s1, Register s2,  Register d ) { v9_only();  Emit( op(arith_op) | rd(d) | op3(movr_op3) | rs1(s1) | rcond(c) | rs2(s2) ); }
  void movr( RCondition c, Register s1, int simm10a,  Register d ) { v9_only();  Emit( op(arith_op) | rd(d) | op3(movr_op3) | rs1(s1) | rcond(c) | immed(true) | simm(simm10a, 10) ); }

  // pp 196

  void mulx(  Register s1, Register s2, Register d ) { v9_only(); Emit( op(arith_op) | rd(d) | op3(mulx_op3 ) | rs1(s1) | rs2(s2) ); }
  void mulx(  Register s1, int simm13a, Register d ) { v9_only(); Emit( op(arith_op) | rd(d) | op3(mulx_op3 ) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void sdivx( Register s1, Register s2, Register d ) { v9_only(); Emit( op(arith_op) | rd(d) | op3(sdivx_op3) | rs1(s1) | rs2(s2) ); }
  void sdivx( Register s1, int simm13a, Register d ) { v9_only(); Emit( op(arith_op) | rd(d) | op3(sdivx_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void udivx( Register s1, Register s2, Register d ) { v9_only(); Emit( op(arith_op) | rd(d) | op3(udivx_op3) | rs1(s1) | rs2(s2) ); }
  void udivx( Register s1, int simm13a, Register d ) { v9_only(); Emit( op(arith_op) | rd(d) | op3(udivx_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }

  // pp 197

  void umul(   Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(umul_op3             ) | rs1(s1) | rs2(s2) ); }
  void umul(   Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(umul_op3             ) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void smul(   Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(smul_op3             ) | rs1(s1) | rs2(s2) ); }
  void smul(   Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(smul_op3             ) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void umulcc( Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(umul_op3 | cc_bit_op3) | rs1(s1) | rs2(s2) ); }
  void umulcc( Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(umul_op3 | cc_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void smulcc( Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(smul_op3 | cc_bit_op3) | rs1(s1) | rs2(s2) ); }
  void smulcc( Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(smul_op3 | cc_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }

  // pp 201

  void nop() { Emit( op(branch_op) | op2(sethi_op2) ); }

  void sw_count() { Emit( op(branch_op) | op2(sethi_op2) | 0x3f0 ); }

  // pp 202

  void popc( Register s,  Register d) { v9_only();  Emit( op(arith_op) | rd(d) | op3(popc_op3) | rs2(s)); }
  void popc( int simm13a, Register d) { v9_only();  Emit( op(arith_op) | rd(d) | op3(popc_op3) | immed(true) | simm(simm13a, 13)); }

  // pp 203

  void prefetch(   Register s1, Register s2, PrefetchFcn f) { v9_only();  Emit( op(ldst_op) | fcn(f) | op3(prefetch_op3) | rs1(s1) | rs2(s2) ); }
  void prefetch(   Register s1, int simm13a, PrefetchFcn f) { v9_only();  Emit( op(ldst_op) | fcn(f) | op3(prefetch_op3) | rs1(s1) | immed(true) | simm(simm13a, 13)); }

  void prefetcha(  Register s1, Register s2, int ia, PrefetchFcn f ) { v9_only();  Emit( op(ldst_op) | fcn(f) | op3(prefetch_op3 | alt_bit_op3) | rs1(s1) | imm_asi(ia) | rs2(s2) ); }
  void prefetcha(  Register s1, int simm13a,         PrefetchFcn f ) { v9_only();  Emit( op(ldst_op) | fcn(f) | op3(prefetch_op3 | alt_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }

  // pp 208

  // not implementing read privileged register

  inline void rdy(    Register d) { v9_dep();  Emit( op(arith_op) | rd(d) | op3(rdreg_op3) | u_field(0, 18, 14)); }
  inline void rdccr(  Register d) { v9_only(); Emit( op(arith_op) | rd(d) | op3(rdreg_op3) | u_field(2, 18, 14)); }
  inline void rdasi(  Register d) { v9_only(); Emit( op(arith_op) | rd(d) | op3(rdreg_op3) | u_field(3, 18, 14)); }
  inline void rdtick( Register d) { v9_only(); Emit( op(arith_op) | rd(d) | op3(rdreg_op3) | u_field(4, 18, 14)); } // Spoon!
  inline void rdpc(   Register d) { v9_only(); Emit( op(arith_op) | rd(d) | op3(rdreg_op3) | u_field(5, 18, 14)); }
  inline void rdfprs( Register d) { v9_only(); Emit( op(arith_op) | rd(d) | op3(rdreg_op3) | u_field(6, 18, 14)); }

  // pp 213

  inline void rett( Register s1, Register s2);
  inline void rett( Register s1, int simm13a/*, relocInfo::relocType rt = relocInfo::none*/);

  // pp 214

  void save(    Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(save_op3) | rs1(s1) | rs2(s2) ); }
  void save(    Register s1, int simm13a, Register d ) {
    // make sure frame is at least large enough for the register save area
   DCHECK(-simm13a >= 16 * wordSize);//, "frame too small");
    Emit( op(arith_op) | rd(d) | op3(save_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) );
  }

  void restore( Register s1 = g0,  Register s2 = g0, Register d = g0 ) { Emit( op(arith_op) | rd(d) | op3(restore_op3) | rs1(s1) | rs2(s2) ); }
  void restore( Register s1,       int simm13a,      Register d      ) { Emit( op(arith_op) | rd(d) | op3(restore_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }

  // pp 216

  void saved()    { v9_only();  Emit( op(arith_op) | fcn(0) | op3(saved_op3)); }
  void restored() { v9_only();  Emit( op(arith_op) | fcn(1) | op3(saved_op3)); }

  // pp 217

  inline void sethi( int imm22a, Register d/*, RelocationHolder const& rspec = RelocationHolder() */);
  // pp 218

  void sll(  Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(sll_op3) | rs1(s1) | sx(0) | rs2(s2) ); }
  void sll(  Register s1, int imm5a,   Register d ) { Emit( op(arith_op) | rd(d) | op3(sll_op3) | rs1(s1) | sx(0) | immed(true) | u_field(imm5a, 4, 0) ); }
  void srl(  Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(srl_op3) | rs1(s1) | sx(0) | rs2(s2) ); }
  void srl(  Register s1, int imm5a,   Register d ) { Emit( op(arith_op) | rd(d) | op3(srl_op3) | rs1(s1) | sx(0) | immed(true) | u_field(imm5a, 4, 0) ); }
  void sra(  Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(sra_op3) | rs1(s1) | sx(0) | rs2(s2) ); }
  void sra(  Register s1, int imm5a,   Register d ) { Emit( op(arith_op) | rd(d) | op3(sra_op3) | rs1(s1) | sx(0) | immed(true) | u_field(imm5a, 4, 0) ); }

  void sllx( Register s1, Register s2, Register d ) { v9_only();  Emit( op(arith_op) | rd(d) | op3(sll_op3) | rs1(s1) | sx(1) | rs2(s2) ); }
  void sllx( Register s1, int imm6a,   Register d ) { v9_only();  Emit( op(arith_op) | rd(d) | op3(sll_op3) | rs1(s1) | sx(1) | immed(true) | u_field(imm6a, 5, 0) ); }
  void srlx( Register s1, Register s2, Register d ) { v9_only();  Emit( op(arith_op) | rd(d) | op3(srl_op3) | rs1(s1) | sx(1) | rs2(s2) ); }
  void srlx( Register s1, int imm6a,   Register d ) { v9_only();  Emit( op(arith_op) | rd(d) | op3(srl_op3) | rs1(s1) | sx(1) | immed(true) | u_field(imm6a, 5, 0) ); }
  void srax( Register s1, Register s2, Register d ) { v9_only();  Emit( op(arith_op) | rd(d) | op3(sra_op3) | rs1(s1) | sx(1) | rs2(s2) ); }
  void srax( Register s1, int imm6a,   Register d ) { v9_only();  Emit( op(arith_op) | rd(d) | op3(sra_op3) | rs1(s1) | sx(1) | immed(true) | u_field(imm6a, 5, 0) ); }

  // pp 220

  void sir( int simm13a ) { Emit( op(arith_op) | fcn(15) | op3(sir_op3) | immed(true) | simm(simm13a, 13)); }

  // pp 221

  void stbar() { Emit( op(arith_op) | op3(membar_op3) | u_field(15, 18, 14)); }

  // pp 222

  inline void stf(    FloatRegister::Width w, FloatRegister d, Register s1, Register s2);
  inline void stf(    FloatRegister::Width w, FloatRegister d, Register s1, int simm13a);

  inline void stfsr(  Register s1, Register s2 );
  inline void stfsr(  Register s1, int simm13a);
  inline void stxfsr( Register s1, Register s2 );
  inline void stxfsr( Register s1, int simm13a);

  //  pp 224

  void stfa(  FloatRegister::Width w, FloatRegister d, Register s1, Register s2, int ia ) { v9_only();  Emit( op(ldst_op) | fd(d, w) | alt_op3(stf_op3 | alt_bit_op3, w) | rs1(s1) | imm_asi(ia) | rs2(s2) ); }
  void stfa(  FloatRegister::Width w, FloatRegister d, Register s1, int simm13a         ) { v9_only();  Emit( op(ldst_op) | fd(d, w) | alt_op3(stf_op3 | alt_bit_op3, w) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void stdf( FloatRegister d, Register s1, Register s2) { stf(FloatRegister::D, d, s1, s2); }
  void stdf( FloatRegister d, Register s1, int simm13a ) { stf(FloatRegister::D, d, s1, simm13a); }
  void stdfa( FloatRegister d, Register s1, Register s2, int ia ) { stfa(FloatRegister::D, d, s1, s2, ia); }
  void stdfa( FloatRegister d, Register s1, int simm13a ) { stfa(FloatRegister::D, d, s1, simm13a); }

  // p 226

  inline void stb(  Register d, Register s1, Register s2 );
  inline void stb(  Register d, Register s1, int simm13a);
  inline void sth(  Register d, Register s1, Register s2 );
  inline void sth(  Register d, Register s1, int simm13a);
  inline void stw(  Register d, Register s1, Register s2 );
  inline void stw(  Register d, Register s1, int simm13a);
  inline void stx(  Register d, Register s1, Register s2 );
  inline void stx(  Register d, Register s1, int simm13a);
  inline void std(  Register d, Register s1, Register s2 );
  inline void std(  Register d, Register s1, int simm13a);

  // pp 177

  void stba(  Register d, Register s1, Register s2, int ia ) {             Emit( op(ldst_op) | rd(d) | op3(stb_op3 | alt_bit_op3) | rs1(s1) | imm_asi(ia) | rs2(s2) ); }
  void stba(  Register d, Register s1, int simm13a         ) {             Emit( op(ldst_op) | rd(d) | op3(stb_op3 | alt_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void stha(  Register d, Register s1, Register s2, int ia ) {             Emit( op(ldst_op) | rd(d) | op3(sth_op3 | alt_bit_op3) | rs1(s1) | imm_asi(ia) | rs2(s2) ); }
  void stha(  Register d, Register s1, int simm13a         ) {             Emit( op(ldst_op) | rd(d) | op3(sth_op3 | alt_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void stwa(  Register d, Register s1, Register s2, int ia ) {             Emit( op(ldst_op) | rd(d) | op3(stw_op3 | alt_bit_op3) | rs1(s1) | imm_asi(ia) | rs2(s2) ); }
  void stwa(  Register d, Register s1, int simm13a         ) {             Emit( op(ldst_op) | rd(d) | op3(stw_op3 | alt_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void stxa(  Register d, Register s1, Register s2, int ia ) { v9_only();  Emit( op(ldst_op) | rd(d) | op3(stx_op3 | alt_bit_op3) | rs1(s1) | imm_asi(ia) | rs2(s2) ); }
  void stxa(  Register d, Register s1, int simm13a         ) { v9_only();  Emit( op(ldst_op) | rd(d) | op3(stx_op3 | alt_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void stda(  Register d, Register s1, Register s2, int ia ) {             Emit( op(ldst_op) | rd(d) | op3(std_op3 | alt_bit_op3) | rs1(s1) | imm_asi(ia) | rs2(s2) ); }
  void stda(  Register d, Register s1, int simm13a         ) {             Emit( op(ldst_op) | rd(d) | op3(std_op3 | alt_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }

  // pp 230

  void sub(    Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(sub_op3              ) | rs1(s1) | rs2(s2) ); }
  void sub(    Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(sub_op3              ) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }

  void subcc(  Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(sub_op3 | cc_bit_op3 ) | rs1(s1) | rs2(s2) ); }
  void subcc(  Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(sub_op3 | cc_bit_op3 ) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void subc(   Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(subc_op3             ) | rs1(s1) | rs2(s2) ); }
  void subc(   Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(subc_op3             ) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }
  void subccc( Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(subc_op3 | cc_bit_op3) | rs1(s1) | rs2(s2) ); }
  void subccc( Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(subc_op3 | cc_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }

  // pp 231

  inline void swap( Register s1, Register s2, Register d );
  inline void swap( Register s1, int simm13a, Register d);

  // pp 232

  void swapa(   Register s1, Register s2, int ia, Register d ) { v9_dep();  Emit( op(ldst_op) | rd(d) | op3(swap_op3 | alt_bit_op3) | rs1(s1) | imm_asi(ia) | rs2(s2) ); }
  void swapa(   Register s1, int simm13a,         Register d ) { v9_dep();  Emit( op(ldst_op) | rd(d) | op3(swap_op3 | alt_bit_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }

  // pp 234, note op in book is wrong, see pp 268

  void taddcc(    Register s1, Register s2, Register d ) {            Emit( op(arith_op) | rd(d) | op3(taddcc_op3  ) | rs1(s1) | rs2(s2) ); }
  void taddcc(    Register s1, int simm13a, Register d ) {            Emit( op(arith_op) | rd(d) | op3(taddcc_op3  ) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }

  // pp 235

  void tsubcc(    Register s1, Register s2, Register d ) { Emit( op(arith_op) | rd(d) | op3(tsubcc_op3  ) | rs1(s1) | rs2(s2) ); }
  void tsubcc(    Register s1, int simm13a, Register d ) { Emit( op(arith_op) | rd(d) | op3(tsubcc_op3  ) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }

  // pp 237

  void trap( Condition c, CC cc, Register s1, Register s2 ) { Emit( op(arith_op) | cond(c) | op3(trap_op3) | rs1(s1) | trapcc(cc) | rs2(s2)); }
  void trap( Condition c, CC cc, Register s1, int trapa   ) { Emit( op(arith_op) | cond(c) | op3(trap_op3) | rs1(s1) | trapcc(cc) | immed(true) | u_field(trapa, 6, 0)); }
  // simple uncond. trap
  void trap( int trapa ) { trap( always, icc, g0, trapa ); }

  // pp 239 omit write priv register for now

  inline void wry(    Register d) { v9_dep();  Emit( op(arith_op) | rs1(d) | op3(wrreg_op3) | u_field(0, 29, 25)); }
  inline void wrccr(Register s) { v9_only(); Emit( op(arith_op) | rs1(s) | op3(wrreg_op3) | u_field(2, 29, 25)); }
  inline void wrccr(Register s, int simm13a) { v9_only(); Emit( op(arith_op) |
                                                                           rs1(s) |
                                                                           op3(wrreg_op3) |
                                                                           u_field(2, 29, 25) |
                                                                           immed(true) |
                                                                           simm(simm13a, 13)); }
  inline void wrasi(Register d) { v9_only(); Emit( op(arith_op) | rs1(d) | op3(wrreg_op3) | u_field(3, 29, 25)); }
  // wrasi(d, imm) stores (d xor imm) to asi
  inline void wrasi(Register d, int simm13a) { v9_only(); Emit( op(arith_op) | rs1(d) | op3(wrreg_op3) |
                                               u_field(3, 29, 25) | immed(true) | simm(simm13a, 13)); }
  inline void wrfprs( Register d) { v9_only(); Emit( op(arith_op) | rs1(d) | op3(wrreg_op3) | u_field(6, 29, 25)); }

  // pp 297 Synthetic Instructions
  inline void cmp(  Register s1, Register s2 ) { subcc( s1, s2, g0 ); }
  inline void cmp(  Register s1, int simm13a ) { subcc( s1, simm13a, g0 ); }

  inline void tst( Register s ) { orcc( g0, s, g0 ); }

  inline void ret()   { jmpl( i7, 2 * kInstructionSize, g0 ); }
  inline void retl()  { jmpl( o7, 2 * kInstructionSize, g0 ); }
 
  // sign-extend 32 to 64
  inline void signx( Register s, Register d ) { sra( s, g0, d); }
  inline void signx( Register d )             { sra( d, g0, d); }

  inline void not1( Register s, Register d ) { xnor( s, g0, d ); }
  inline void not1( Register d )             { xnor( d, g0, d ); }

  inline void neg( Register s, Register d ) { sub( g0, s, d ); }
  inline void neg( Register d )             { sub( g0, d, d ); }

  inline void cas(  Register s1, Register s2, Register d) { casa( s1, s2, d, ASI_PRIMARY); }
  inline void casx( Register s1, Register s2, Register d) { casxa(s1, s2, d, ASI_PRIMARY); }
  inline void cas_ptr(  Register s1, Register s2, Register d) { casx( s1, s2, d ); }

  // little-endian
  inline void casl(  Register s1, Register s2, Register d) { casa( s1, s2, d, ASI_PRIMARY_LITTLE); }
  inline void casxl( Register s1, Register s2, Register d) { casxa(s1, s2, d, ASI_PRIMARY_LITTLE); }

  inline void inc(   Register d,  int const13 = 1 ) { add(   d, const13, d); }
  inline void inccc( Register d,  int const13 = 1 ) { addcc( d, const13, d); }

  inline void dec(   Register d,  int const13 = 1 ) { sub(   d, const13, d); }
  inline void deccc( Register d,  int const13 = 1 ) { subcc( d, const13, d); }

  inline void btst( Register s1,  Register s2 ) { andcc( s1, s2, g0 ); }
  inline void btst( int simm13a,  Register s )  { andcc( s,  simm13a, g0 ); }

  inline void bset( Register s1,  Register s2 ) { or3( s1, s2, s2 ); }
  inline void bset( int simm13a,  Register s )  { or3( s,  simm13a, s ); }

  inline void bclr( Register s1,  Register s2 ) { andn( s1, s2, s2 ); }
  inline void bclr( int simm13a,  Register s )  { andn( s,  simm13a, s ); }

  inline void btog( Register s1,  Register s2 ) { xor3( s1, s2, s2 ); }
  inline void btog( int simm13a,  Register s )  { xor3( s,  simm13a, s ); }

  inline void clr( Register d ) { or3( g0, g0, d ); }

  inline void clrb( Register s1, Register s2);
  inline void clrh( Register s1, Register s2);
  inline void clr(  Register s1, Register s2);
  inline void clrx( Register s1, Register s2);

  inline void clrb( Register s1, int simm13a);
  inline void clrh( Register s1, int simm13a);
  inline void clr(  Register s1, int simm13a);
  inline void clrx( Register s1, int simm13a);

  // copy & clear upper word
  inline void clruw( Register s, Register d ) { srl( s, g0, d); }
  // clear upper word
  inline void clruwu( Register d ) { srl( d, g0, d); }

   inline void mov( Register s,  Register d) {
    if ( !s.is(d) )
        or3( g0, s, d);
    else
        assert_not_delayed();  // Put something useful in the delay slot!
  }
  inline void mov( int simm13a, Register d) { or3( g0, simm13a, d); }
  
  //  VIS1 instructions

  void alignaddr( Register s1, Register s2, Register d ) { vis1_only(); Emit( op(arith_op) | rd(d) | op3(alignaddr_op3) | rs1(s1) | opf(alignaddr_opf) | rs2(s2)); }

  void faligndata( FloatRegister s1, FloatRegister s2, FloatRegister d ) { vis1_only(); Emit( op(arith_op) | fd(d, FloatRegister::D) | op3(faligndata_op3) | fs1(s1, FloatRegister::D) | opf(faligndata_opf) | fs2(s2, FloatRegister::D)); }

  void fzero( FloatRegister::Width w, FloatRegister d ) { vis1_only(); Emit( op(arith_op) | fd(d, w) | op3(fzero_op3) | opf(0x62 - w)); }

  void fsrc2( FloatRegister::Width w, FloatRegister s2, FloatRegister d ) { vis1_only(); Emit( op(arith_op) | fd(d, w) | op3(fsrc_op3) | opf(0x7A - w) | fs2(s2, w)); }

  void fnot1( FloatRegister::Width w, FloatRegister s1, FloatRegister d ) { vis1_only(); Emit( op(arith_op) | fd(d, w) | op3(fnot_op3) | fs1(s1, w) | opf(0x6C - w)); }

  void fpmerge( FloatRegister s1, FloatRegister s2, FloatRegister d ) { vis1_only(); Emit( op(arith_op) | fd(d, FloatRegister::D) | op3(0x36) | fs1(s1, FloatRegister::S) | opf(0x4b) | fs2(s2, FloatRegister::S)); }

  void stpartialf( Register s1, Register s2, FloatRegister d, int ia = -1 ) { vis1_only(); Emit( op(ldst_op) | fd(d, FloatRegister::D) | op3(stpartialf_op3) | rs1(s1) | imm_asi(ia) | rs2(s2)); }

  //  VIS2 instructions

  void edge8n( Register s1, Register s2, Register d ) { vis2_only(); Emit( op(arith_op) | rd(d) | op3(edge_op3) | rs1(s1) | opf(edge8n_opf) | rs2(s2)); }

  // VIS3 instructions

  void movstosw( FloatRegister s, Register d ) { vis3_only();  Emit( op(arith_op) | rd(d) | op3(mftoi_op3) | opf(mstosw_opf) | fs2(s, FloatRegister::S)); }
  void movstouw( FloatRegister s, Register d ) { vis3_only();  Emit( op(arith_op) | rd(d) | op3(mftoi_op3) | opf(mstouw_opf) | fs2(s, FloatRegister::S)); }
  void movdtox(  FloatRegister s, Register d ) { vis3_only();  Emit( op(arith_op) | rd(d) | op3(mftoi_op3) | opf(mdtox_opf) | fs2(s, FloatRegister::D)); }

  void movwtos( Register s, FloatRegister d ) { vis3_only();  Emit( op(arith_op) | fd(d, FloatRegister::S) | op3(mftoi_op3) | opf(mwtos_opf) | rs2(s)); }
  void movxtod( Register s, FloatRegister d ) { vis3_only();  Emit( op(arith_op) | fd(d, FloatRegister::D) | op3(mftoi_op3) | opf(mxtod_opf) | rs2(s)); }

  void xmulx(Register s1, Register s2, Register d) { vis3_only(); Emit( op(arith_op) | rd(d) | op3(xmulx_op3) | rs1(s1) | opf(xmulx_opf) | rs2(s2)); }
  void xmulxhi(Register s1, Register s2, Register d) { vis3_only(); Emit( op(arith_op) | rd(d) | op3(xmulx_op3) | rs1(s1) | opf(xmulxhi_opf) | rs2(s2)); }

  // Crypto SHA instructions

  void sha1()   { sha1_only();    Emit( op(arith_op) | op3(sha_op3) | opf(sha1_opf)); }
  void sha256() { sha256_only();  Emit( op(arith_op) | op3(sha_op3) | opf(sha256_opf)); }
  void sha512() { sha512_only();  Emit( op(arith_op) | op3(sha_op3) | opf(sha512_opf)); }

  // CRC32C instruction

  void crc32c( FloatRegister s1, FloatRegister s2, FloatRegister d ) { crc32c_only();  Emit( op(arith_op) | fd(d, FloatRegister::D) | op3(crc32c_op3) | fs1(s1, FloatRegister::D) | opf(crc32c_opf) | fs2(s2, FloatRegister::D)); }

  
private:
 
  // Emit the instruction at pc_.
  void Emit(Instr instruction) {
    check_delay();
    STATIC_ASSERT(sizeof(*pc_) == 1);
    STATIC_ASSERT(sizeof(instruction) == kInstructionSize);
    DCHECK((pc_ + sizeof(instruction)) <= (buffer_ + buffer_size_));

    memcpy(pc_, &instruction, sizeof(instruction));
    pc_ += sizeof(instruction);
    CheckBuffer();
  }

  // Emit data inline in the instruction stream.
  void EmitData(void const * data, unsigned size) {
    check_delay();
    DCHECK(sizeof(*pc_) == 1);
    DCHECK((pc_ + size) <= (buffer_ + buffer_size_));

    // TODO(all): Somehow register we have some data here. Then we can
    // disassemble it correctly.
    memcpy(pc_, data, size);
    pc_ += size;
    CheckBuffer();
  }
  
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
