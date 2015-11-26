// Copyright 2006-2013 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// This module contains the architecture-specific code. This make the rest of
// the code less dependent on differences between different processor
// architecture.
// The classes have the same definition for all architectures. The
// implementation for a particular architecture is put in cpu_<arch>.cc.
// The build system then uses the implementation for the target architecture.
//

#ifndef V8_BASE_CPU_H_
#define V8_BASE_CPU_H_

#include "src/base/macros.h"

namespace v8 {
namespace base {

// ----------------------------------------------------------------------------
// CPU
//
// Query information about the processor.
//
// This class also has static methods for the architecture specific functions.
// Add methods here to cope with differences between the supported
// architectures. For each architecture the file cpu_<arch>.cc contains the
// implementation of these static functions.

class CPU final {
 public:
  CPU();

  // x86 CPUID information
  const char* vendor() const { return vendor_; }
  int stepping() const { return stepping_; }
  int model() const { return model_; }
  int ext_model() const { return ext_model_; }
  int family() const { return family_; }
  int ext_family() const { return ext_family_; }
  int type() const { return type_; }

  // arm implementer/part information
  int implementer() const { return implementer_; }
  static const int ARM = 0x41;
  static const int NVIDIA = 0x4e;
  static const int QUALCOMM = 0x51;
  int architecture() const { return architecture_; }
  int variant() const { return variant_; }
  static const int NVIDIA_DENVER = 0x0;
  int part() const { return part_; }

  // ARM-specific part codes
  static const int ARM_CORTEX_A5 = 0xc05;
  static const int ARM_CORTEX_A7 = 0xc07;
  static const int ARM_CORTEX_A8 = 0xc08;
  static const int ARM_CORTEX_A9 = 0xc09;
  static const int ARM_CORTEX_A12 = 0xc0c;
  static const int ARM_CORTEX_A15 = 0xc0f;

  // Denver-specific part code
  static const int NVIDIA_DENVER_V10 = 0x002;

  // PPC-specific part codes
  enum {
    PPC_POWER5,
    PPC_POWER6,
    PPC_POWER7,
    PPC_POWER8,
    PPC_G4,
    PPC_G5,
    PPC_PA6T
  };
 
  //SPARC Features
   enum {
    v8_instructions      = 0,
    hardware_mul32       = 1,
    hardware_div32       = 2,
    hardware_fsmuld      = 3,
    hardware_popc        = 4,
    v9_instructions      = 5,
    vis1_instructions    = 6,
    vis2_instructions    = 7,
    sun4v_instructions   = 8,
    blk_init_instructions = 9,
    fmaf_instructions    = 10,
    fmau_instructions    = 11,
    vis3_instructions    = 12,
    cbcond_instructions  = 13,
    sparc64_family       = 14,
    M_family             = 15,
    T_family             = 16,
    T1_model             = 17,
    sparc5_instructions  = 18,
    aes_instructions     = 19,
    sha1_instruction     = 20,
    sha256_instruction   = 21,
    sha512_instruction   = 22,
    crc32c_instruction   = 23
  };

 //SPARC Feature Masks
   enum  {
    unknown_m           = 0,
    allfeatures__m      = -1,

    v8_instructions_m       = 1 << v8_instructions,
    hardware_mul32_m        = 1 << hardware_mul32,
    hardware_div32_m        = 1 << hardware_div32,
    hardware_fsmuld_m       = 1 << hardware_fsmuld,
    hardware_popc_m         = 1 << hardware_popc,
    v9_instructions_m       = 1 << v9_instructions,
    vis1_instructions_m     = 1 << vis1_instructions,
    vis2_instructions_m     = 1 << vis2_instructions,
    sun4v_m                 = 1 << sun4v_instructions,
    blk_init_instructions_m = 1 << blk_init_instructions,
    fmaf_instructions_m     = 1 << fmaf_instructions,
    fmau_instructions_m     = 1 << fmau_instructions,
    vis3_instructions_m     = 1 << vis3_instructions,
    cbcond_instructions_m   = 1 << cbcond_instructions,
    sparc64_family_m        = 1 << sparc64_family,
    M_family_m              = 1 << M_family,
    T_family_m              = 1 << T_family,
    T1_model_m              = 1 << T1_model,
    sparc5_instructions_m   = 1 << sparc5_instructions,
    aes_instructions_m      = 1 << aes_instructions,
    sha1_instruction_m      = 1 << sha1_instruction,
    sha256_instruction_m    = 1 << sha256_instruction,
    sha512_instruction_m    = 1 << sha512_instruction,
    crc32c_instruction_m    = 1 << crc32c_instruction,

    generic_v8_m        = v8_instructions_m | hardware_mul32_m | hardware_div32_m | hardware_fsmuld_m,
    generic_v9_m        = generic_v8_m | v9_instructions_m,
    ultra3_m            = generic_v9_m | vis1_instructions_m | vis2_instructions_m,

    // Temporary until we have something more accurate
    niagara1_unique_m   = sun4v_m,
    niagara1_m          = generic_v9_m | niagara1_unique_m
  };


  // General features
  bool has_fpu() const { return has_fpu_; }

  // x86 features
  bool has_cmov() const { return has_cmov_; }
  bool has_sahf() const { return has_sahf_; }
  bool has_mmx() const { return has_mmx_; }
  bool has_sse() const { return has_sse_; }
  bool has_sse2() const { return has_sse2_; }
  bool has_sse3() const { return has_sse3_; }
  bool has_ssse3() const { return has_ssse3_; }
  bool has_sse41() const { return has_sse41_; }
  bool has_sse42() const { return has_sse42_; }
  bool has_osxsave() const { return has_osxsave_; }
  bool has_avx() const { return has_avx_; }
  bool has_fma3() const { return has_fma3_; }
  bool has_bmi1() const { return has_bmi1_; }
  bool has_bmi2() const { return has_bmi2_; }
  bool has_lzcnt() const { return has_lzcnt_; }
  bool has_popcnt() const { return has_popcnt_; }
  bool is_atom() const { return is_atom_; }

  // arm features
  bool has_idiva() const { return has_idiva_; }
  bool has_neon() const { return has_neon_; }
  bool has_thumb2() const { return has_thumb2_; }
  bool has_vfp() const { return has_vfp_; }
  bool has_vfp3() const { return has_vfp3_; }
  bool has_vfp3_d32() const { return has_vfp3_d32_; }

  // mips features
  bool is_fp64_mode() const { return is_fp64_mode_; }

  //sparc features
  bool has_v8()                  { return (features_ & v8_instructions_m) != 0; }
  bool has_v9()                  { return (features_ & v9_instructions_m) != 0; }
  bool has_hardware_mul32()      { return (features_ & hardware_mul32_m) != 0; }
  bool has_hardware_div32()      { return (features_ & hardware_div32_m) != 0; }
  bool has_hardware_fsmuld()     { return (features_ & hardware_fsmuld_m) != 0; }
  bool has_hardware_popc()       { return (features_ & hardware_popc_m) != 0; }
  bool has_vis1()                { return (features_ & vis1_instructions_m) != 0; }
  bool has_vis2()                { return (features_ & vis2_instructions_m) != 0; }
  bool has_vis3()                { return (features_ & vis3_instructions_m) != 0; }
  bool has_blk_init()            { return (features_ & blk_init_instructions_m) != 0; }
  bool has_cbcond()              { return (features_ & cbcond_instructions_m) != 0; }
  bool has_sparc5_instr()        { return (features_ & sparc5_instructions_m) != 0; }
  bool has_aes()                 { return (features_ & aes_instructions_m) != 0; }
  bool has_sha1()                { return (features_ & sha1_instruction_m) != 0; }
  bool has_sha256()              { return (features_ & sha256_instruction_m) != 0; }
  bool has_sha512()              { return (features_ & sha512_instruction_m) != 0; }
  bool has_crc32c()              { return (features_ & crc32c_instruction_m) != 0; }

  bool supports_compare_and_exchange() { return has_v9(); }

  // Returns true if the platform is in the niagara line (T series)
  // and newer than the niagara1.
  bool is_niagara_plus()         { return is_T_family(features_) && !is_T1_model(features_); }

  bool is_M_series()             { return is_M_family(features_); }
  bool is_T4()                   { return is_T_family(features_) && has_cbcond(); }
  bool is_T7()                   { return is_T_family(features_) && has_sparc5_instr(); }

  // Fujitsu SPARC64
  bool is_sparc64()              { return (features_ & sparc64_family_m) != 0; }

  bool is_sun4v()                { return (features_ & sun4v_m) != 0; }
  bool is_ultra3()               { return (features_ & ultra3_m) == ultra3_m && !is_sun4v() && !is_sparc64(); }

  bool has_fast_fxtof()          { return (is_niagara() || is_sparc64() || has_v9()) && !is_ultra3(); }
  bool has_fast_idiv()           { return is_niagara_plus() || is_sparc64(); }

  // T4 and newer Sparc have fast RDPC instruction.
  bool has_fast_rdpc()           { return is_T4(); }

  // On T4 and newer Sparc BIS to the beginning of cache line always zeros it.
  bool has_block_zeroing()       { return has_blk_init() && is_T4(); }

protected:
    
  // Returns true if the platform is in the niagara line (T series)
  bool is_M_family(int features) { return (features & M_family_m) != 0; }
  bool is_T_family(int features) { return (features & T_family_m) != 0; }
  bool is_niagara() { return is_T_family(features_); }
#ifdef DEBUG
  bool is_niagara(int features)  {
    // 'sun4v_m' may be defined on both Sun/Oracle Sparc CPUs as well as
    // on Fujitsu Sparc64 CPUs, but only Sun/Oracle Sparcs can be 'niagaras'.
    return (features & sun4v_m) != 0 && (features & sparc64_family_m) == 0;
  }
#endif

  // Returns true if it is niagara1 (T1).
  bool is_T1_model(int features) { return is_T_family(features) && ((features & T1_model_m) != 0); }

private:
  char vendor_[13];
  int stepping_;
  int model_;
  int ext_model_;
  int family_;
  int ext_family_;
  int type_;
  int implementer_;
  int architecture_;
  int variant_;
  int part_;
  int features_;
  bool has_fpu_;
  bool has_cmov_;
  bool has_sahf_;
  bool has_mmx_;
  bool has_sse_;
  bool has_sse2_;
  bool has_sse3_;
  bool has_ssse3_;
  bool has_sse41_;
  bool has_sse42_;
  bool is_atom_;
  bool has_osxsave_;
  bool has_avx_;
  bool has_fma3_;
  bool has_bmi1_;
  bool has_bmi2_;
  bool has_lzcnt_;
  bool has_popcnt_;
  bool has_idiva_;
  bool has_neon_;
  bool has_thumb2_;
  bool has_vfp_;
  bool has_vfp3_;
  bool has_vfp3_d32_;
  bool is_fp64_mode_;
  bool has_mul32_;
  bool has_div32_;
  bool has_fsmuld_;
  bool has_v9_;
  bool has_popc_;
  bool has_vis1_;
  bool has_vis2_;
  bool has_vis3_;
  bool has_blk_init_;
  bool has_fmaf_;
  bool has_cbcond_;
};

}  // namespace base
}  // namespace v8

#endif  // V8_BASE_CPU_H_
