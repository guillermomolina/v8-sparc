// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_SPARC_CONSTANTS_SPARC_H_
#define V8_SPARC_CONSTANTS_SPARC_H_

#include <stdint.h>

#include "src/base/logging.h"
#include "src/base/macros.h"
#include "src/globals.h"

namespace v8 {
namespace internal {

// Number of registers
const int kNumRegisters = 32;

// FP support.
const int kNumDoubleRegisters = 64;

    
#ifdef  _LP64
#define LP64_ONLY(code) code
#define NOT_LP64(code)
    const unsigned kStackBias = 2047;
    const unsigned kMinimumFrameSize = 176;
    const unsigned kInstructionsPerPachableSet = 8;
#else  // !_LP64
#define LP64_ONLY(code)
#define NOT_LP64(code) code
    const unsigned kStackBias = 0;
    const unsigned kMinimumFrameSize = 96;
    const unsigned kInstructionsPerPachableSet = 2;
#endif // _LP64
  
      
/*  
 ---- <- FP Points HERE in SPARC
  kCallerSPOffset = kCallerPCOffset + 1 * kPCOnStackSize;
  kCallerPCOffset = +1 * kFPOnStackSize;
  kCallerFPOffset = 0 * kPointerSize;  <-FP points here in other platforms
  kContextOffset = -1 * kPointerSize;
  kMarkerOffset = -2 * kPointerSize;
  kCodeOffset = -3 * kPointerSize;
*/

// difference between SPARC and others frame
 const int kSparcFrameBias = kStackBias - 3 * kPointerSize;
 
const int kWordSize = sizeof(char*);

// On SPARC all instructions are 32 bits.
typedef int32_t Instr;

const unsigned kInstructionSize = 4;
const unsigned kInstructionSizeLog2 = 2;

 enum ops {
    call_op   = 1, // fmt 1
    branch_op = 0, // also sethi (fmt2)
    arith_op  = 2, // fmt 3, arith & misc
    ldst_op   = 3  // fmt 3, load/store
  };

  enum op2s {
    bpr_op2   = 3,
    fb_op2    = 6,
    fbp_op2   = 5,
    br_op2    = 2,
    bp_op2    = 1,
    sethi_op2 = 4
  };

  enum op3s {
    // selected op3s
    add_op3      = 0x00,
    and_op3      = 0x01,
    or_op3       = 0x02,
    xor_op3      = 0x03,
    sub_op3      = 0x04,
    andn_op3     = 0x05,
    orn_op3      = 0x06,
    xnor_op3     = 0x07,
    addc_op3     = 0x08,
    mulx_op3     = 0x09,
    umul_op3     = 0x0a,
    smul_op3     = 0x0b,
    subc_op3     = 0x0c,
    udivx_op3    = 0x0d,
    udiv_op3     = 0x0e,
    sdiv_op3     = 0x0f,

    addcc_op3    = 0x10,
    andcc_op3    = 0x11,
    orcc_op3     = 0x12,
    xorcc_op3    = 0x13,
    subcc_op3    = 0x14,
    andncc_op3   = 0x15,
    orncc_op3    = 0x16,
    xnorcc_op3   = 0x17,
    addccc_op3   = 0x18,
    aes4_op3     = 0x19,
    umulcc_op3   = 0x1a,
    smulcc_op3   = 0x1b,
    subccc_op3   = 0x1c,
    udivcc_op3   = 0x1e,
    sdivcc_op3   = 0x1f,

    taddcc_op3   = 0x20,
    tsubcc_op3   = 0x21,
    taddcctv_op3 = 0x22,
    tsubcctv_op3 = 0x23,
    mulscc_op3   = 0x24,
    sll_op3      = 0x25,
    sllx_op3     = 0x25,
    srl_op3      = 0x26,
    srlx_op3     = 0x26,
    sra_op3      = 0x27,
    srax_op3     = 0x27,
    rdreg_op3    = 0x28,
    membar_op3   = 0x28,

    flushw_op3   = 0x2b,
    movcc_op3    = 0x2c,
    sdivx_op3    = 0x2d,
    popc_op3     = 0x2e,
    movr_op3     = 0x2f,

    sir_op3      = 0x30,
    wrreg_op3    = 0x30,
    saved_op3    = 0x31,

    fpop1_op3    = 0x34,
    fpop2_op3    = 0x35,
    impdep1_op3  = 0x36,
    aes3_op3     = 0x36,
    sha_op3      = 0x36,
    alignaddr_op3  = 0x36,
    faligndata_op3 = 0x36,
    flog3_op3    = 0x36,
    edge_op3     = 0x36,
    fzero_op3    = 0x36,
    fsrc_op3     = 0x36,
    fnot_op3     = 0x36,
    xmulx_op3    = 0x36,
    crc32c_op3   = 0x36,
    impdep2_op3  = 0x37,
    stpartialf_op3 = 0x37,
    jmpl_op3     = 0x38,
    rett_op3     = 0x39,
    trap_op3     = 0x3a,
    flush_op3    = 0x3b,
    save_op3     = 0x3c,
    restore_op3  = 0x3d,
    done_op3     = 0x3e,
    retry_op3    = 0x3e,

    lduw_op3     = 0x00,
    ldub_op3     = 0x01,
    lduh_op3     = 0x02,
    ldd_op3      = 0x03,
    stw_op3      = 0x04,
    stb_op3      = 0x05,
    sth_op3      = 0x06,
    std_op3      = 0x07,
    ldsw_op3     = 0x08,
    ldsb_op3     = 0x09,
    ldsh_op3     = 0x0a,
    ldx_op3      = 0x0b,

    stx_op3      = 0x0e,
    swap_op3     = 0x0f,

    stwa_op3     = 0x14,
    stxa_op3     = 0x1e,

    ldf_op3      = 0x20,
    ldfsr_op3    = 0x21,
    ldqf_op3     = 0x22,
    lddf_op3     = 0x23,
    stf_op3      = 0x24,
    stfsr_op3    = 0x25,
    stqf_op3     = 0x26,
    stdf_op3     = 0x27,

    prefetch_op3 = 0x2d,

    casa_op3     = 0x3c,
    casxa_op3    = 0x3e,

    mftoi_op3    = 0x36,

    alt_bit_op3  = 0x10,
     cc_bit_op3  = 0x10
  };

  enum opfs {
    // selected opfs
    edge8n_opf         = 0x01,

    fmovs_opf          = 0x01,
    fmovd_opf          = 0x02,

    fnegs_opf          = 0x05,
    fnegd_opf          = 0x06,

    alignaddr_opf      = 0x18,

    fadds_opf          = 0x41,
    faddd_opf          = 0x42,
    fsubs_opf          = 0x45,
    fsubd_opf          = 0x46,

    faligndata_opf     = 0x48,

    fmuls_opf          = 0x49,
    fmuld_opf          = 0x4a,
    fdivs_opf          = 0x4d,
    fdivd_opf          = 0x4e,

    fcmps_opf          = 0x51,
    fcmpd_opf          = 0x52,

    fstox_opf          = 0x81,
    fdtox_opf          = 0x82,
    fxtos_opf          = 0x84,
    fxtod_opf          = 0x88,
    fitos_opf          = 0xc4,
    fdtos_opf          = 0xc6,
    fitod_opf          = 0xc8,
    fstod_opf          = 0xc9,
    fstoi_opf          = 0xd1,
    fdtoi_opf          = 0xd2,

    mdtox_opf          = 0x110,
    mstouw_opf         = 0x111,
    mstosw_opf         = 0x113,
    xmulx_opf          = 0x115,
    xmulxhi_opf        = 0x116,
    mxtod_opf          = 0x118,
    mwtos_opf          = 0x119,

    aes_kexpand0_opf   = 0x130,
    aes_kexpand2_opf   = 0x131,

    sha1_opf           = 0x141,
    sha256_opf         = 0x142,
    sha512_opf         = 0x143,

    crc32c_opf         = 0x147
  };

  enum op5s {
    aes_eround01_op5     = 0x00,
    aes_eround23_op5     = 0x01,
    aes_dround01_op5     = 0x02,
    aes_dround23_op5     = 0x03,
    aes_eround01_l_op5   = 0x04,
    aes_eround23_l_op5   = 0x05,
    aes_dround01_l_op5   = 0x06,
    aes_dround23_l_op5   = 0x07,
    aes_kexpand1_op5     = 0x08
  };

  enum RCondition {  rc_z = 1,  rc_lez = 2,  rc_lz = 3, rc_nz = 5, rc_gz = 6, rc_gez = 7, rc_last = rc_gez  };

     // for FBfcc & FBPfcc instruction
enum FPUCondition {
  f_never                     = 0,
  f_notEqual                  = 1,
  f_notZero                   = 1,
  f_lessOrGreater             = 2,
  f_unorderedOrLess           = 3,
  f_less                      = 4,
  f_unorderedOrGreater        = 5,
  f_greater                   = 6,
  f_unordered                 = 7,
  f_always                    = 8,
  f_equal                     = 9,
  f_zero                      = 9,
  f_unorderedOrEqual          = 10,
  f_greaterOrEqual            = 11,
  f_unorderedOrGreaterOrEqual = 12,
  f_lessOrEqual               = 13,
  f_unorderedOrLessOrEqual    = 14,
  f_ordered                   = 15,
};

    // for integers
 enum Condition {
  never                 =  0,
  equal                 =  1,
  zero                  =  1,
  lessEqual             =  2,
  less                  =  3,
  lessEqualUnsigned     =  4,
  lessUnsigned          =  5,
  carrySet              =  5,
  negative              =  6,
  overflowSet           =  7,
  always                =  8,
  notEqual              =  9,
  notZero               =  9,
  greater               =  10,
  greaterEqual          =  11,
  greaterUnsigned       =  12,
  greaterEqualUnsigned  =  13,
  carryClear            =  13,
  positive              =  14,
  overflowClear         =  15
};

  enum CC {
    icc  = 0,  xcc  = 2,
    // ptr_cc is the correct condition code for a pointer or int:
    ptr_cc = NOT_LP64(icc) LP64_ONLY(xcc),
    fcc0 = 0,  fcc1 = 1, fcc2 = 2, fcc3 = 3
  };

  enum PrefetchFcn {
    severalReads = 0,  oneRead = 1,  severalWritesAndPossiblyReads = 2, oneWrite = 3, page = 4
  };

  enum Predict { pt = 1, pn = 0 }; // pt = predict taken

  enum Membar_mask_bits { // page 184, v9
    StoreStore = 1 << 3,
    LoadStore  = 1 << 2,
    StoreLoad  = 1 << 1,
    LoadLoad   = 1 << 0,

    Sync       = 1 << 6,
    MemIssue   = 1 << 5,
    Lookaside  = 1 << 4
  };

  enum ASIs { // page 72, v9
    ASI_PRIMARY            = 0x80,
    ASI_PRIMARY_NOFAULT    = 0x82,
    ASI_PRIMARY_LITTLE     = 0x88,
    // 8x8-bit partial store
    ASI_PST8_PRIMARY       = 0xC0,
    // Block initializing store
    ASI_ST_BLKINIT_PRIMARY = 0xE2,
    // Most-Recently-Used (MRU) BIS variant
    ASI_ST_BLKINIT_MRU_PRIMARY = 0xF2
    // add more from book as needed
  };
/*
// -----------------------------------------------------------------------------
// Instruction abstraction.

// The class Instruction enables access to individual fields defined in the PPC
// architecture instruction set encoding.
// Note that the Assembler uses typedef int32_t Instr.
//
// Example: Test whether the instruction at ptr does set the condition code
// bits.
//
// bool InstructionSetsConditionCodes(byte* ptr) {
//   Instruction* instr = Instruction::At(ptr);
//   int type = instr->TypeValue();
//   return ((type == 0) || (type == 1)) && instr->HasS();
// }
//
class Instruction {
 public:
  enum { kInstrSize = 4, kInstrSizeLog2 = 2, kPCReadOffset = 8 };

// Helper macro to define static accessors.
// We use the cast to char* trick to bypass the strict anti-aliasing rules.
#define DECLARE_STATIC_TYPED_ACCESSOR(return_type, Name) \
  static inline return_type Name(Instr instr) {          \
    char* temp = reinterpret_cast<char*>(&instr);        \
    return reinterpret_cast<Instruction*>(temp)->Name(); \
  }

#define DECLARE_STATIC_ACCESSOR(Name) DECLARE_STATIC_TYPED_ACCESSOR(int, Name)

  // Get the raw instruction bits.
  inline Instr InstructionBits() const {
    return *reinterpret_cast<const Instr*>(this);
  }

  // Set the raw instruction bits to value.
  inline void SetInstructionBits(Instr value) {
    *reinterpret_cast<Instr*>(this) = value;
  }

  // Read one particular bit out of the instruction bits.
  inline int Bit(int nr) const { return (InstructionBits() >> nr) & 1; }

  // Read a bit field's value out of the instruction bits.
  inline int Bits(int hi, int lo) const {
    return (InstructionBits() >> lo) & ((2 << (hi - lo)) - 1);
  }

  // Read a bit field out of the instruction bits.
  inline int BitField(int hi, int lo) const {
    return InstructionBits() & (((2 << (hi - lo)) - 1) << lo);
  }

  // Static support.

  // Read one particular bit out of the instruction bits.
  static inline int Bit(Instr instr, int nr) { return (instr >> nr) & 1; }

  // Read the value of a bit field out of the instruction bits.
  static inline int Bits(Instr instr, int hi, int lo) {
    return (instr >> lo) & ((2 << (hi - lo)) - 1);
  }


  // Read a bit field out of the instruction bits.
  static inline int BitField(Instr instr, int hi, int lo) {
    return instr & (((2 << (hi - lo)) - 1) << lo);
  }

  // Instructions are read of out a code stream. The only way to get a
  // reference to an instruction is to convert a pointer. There is no way
  // to allocate or create instances of class Instruction.
  // Use the At(pc) function to create references to Instruction.
  static Instruction* At(byte* pc) {
    return reinterpret_cast<Instruction*>(pc);
  }


 private:
  // We need to prevent the creation of instances of class Instruction.
  DISALLOW_IMPLICIT_CONSTRUCTORS(Instruction);
};
*/

// Helper functions for converting between register numbers and names.
class Registers {
 public:
  // Lookup the register number for the name provided.
  static int Number(const char* name);

 private:
  static const char* names_[kNumRegisters];
};

// Helper functions for converting between FP register numbers and names.
class DoubleRegisters {
 public:
  // Lookup the register number for the name provided.
  static int Number(const char* name);

 private:
  static const char* names_[kNumDoubleRegisters];
};

}  // namespace internal
}  // namespace v8

#endif  // V8_SPARC_CONSTANTS_SPARC_H_
