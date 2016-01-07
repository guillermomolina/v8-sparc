// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_SPARC_MACRO_ASSEMBLER_SPARC_H_
#define V8_SPARC_MACRO_ASSEMBLER_SPARC_H_

#include "src/assembler.h"
#include "src/bailout-reason.h"
#include "src/frames.h"
#include "src/globals.h"

namespace v8 {
namespace internal {

// <sys/trap.h> promises that the system will not use traps 16-31
#define ST_BREAKPOINT 0x1

// Give alias names to registers for calling conventions.
const Register kReturnRegister0 = {Register::kCode_i0};
const Register kReturnRegister1 = {Register::kCode_i1};
const Register kJSFunctionRegister = {Register::kCode_g4};
const Register kContextRegister = {Register::kCode_g5};
const Register kInterpreterAccumulatorRegister = {Register::kCode_l0};
const Register kInterpreterRegisterFileRegister = {Register::kCode_l1};
const Register kInterpreterBytecodeOffsetRegister = {Register::kCode_l2};
const Register kInterpreterBytecodeArrayRegister = {Register::kCode_l3};
const Register kInterpreterDispatchTableRegister = {Register::kCode_l4};
const Register kJavaScriptCallArgCountRegister = {Register::kCode_i0};
const Register kJavaScriptCallNewTargetRegister = {Register::kCode_i2};
const Register kRuntimeCallFunctionRegister = {Register::kCode_i1};
const Register kRuntimeCallArgCountRegister = {Register::kCode_i0};

const Register kScratchRegister = { Register::kCode_g1 };

// Flags used for AllocateHeapNumber
enum TaggingMode {
  // Tag the result.
  TAG_RESULT,
  // Don't tag
  DONT_TAG_RESULT
};

enum RememberedSetAction { EMIT_REMEMBERED_SET, OMIT_REMEMBERED_SET };
enum SmiCheck { INLINE_SMI_CHECK, OMIT_SMI_CHECK };
enum PointersToHereCheck {
  kPointersToHereMaybeInteresting,
  kPointersToHereAreAlwaysInteresting
};
enum LinkRegisterStatus { kLRHasNotBeenSaved, kLRHasBeenSaved };
enum RAStatus { kRAHasNotBeenSaved, kRAHasBeenSaved };

// -----------------------------------------------------------------------------
// Machine instruction Operands.
const int kSmiShift = kSmiTagSize + kSmiShiftSize;
const uint64_t kSmiShiftMask = (1UL << kSmiShift) - 1;
// Class Operand represents a shifter operand in data processing instructions.
class Operand BASE_EMBEDDED {
 public:
  // Immediate.
  INLINE(explicit Operand(int64_t immediate,
         RelocInfo::Mode rmode = RelocInfo::NONE64));
  INLINE(explicit Operand(const ExternalReference& f));
  INLINE(explicit Operand(const char* s));
  INLINE(explicit Operand(Object** opp));
  INLINE(explicit Operand(Context** cpp));
  explicit Operand(Handle<Object> handle);
  INLINE(explicit Operand(Smi* value));

  // Register.
  INLINE(explicit Operand(Register rm));

  // Return true if this is a register operand.
  INLINE(bool is_reg() const);

  bool must_output_reloc_info(const MacroAssembler* masm) const;

  inline int64_t immediate() const {
    DCHECK(!is_reg());
    return imm64_;
  }

  Register rm() const { return rm_; }

 private:
  Register rm_;
  int64_t imm64_;  // Valid if rm_ == no_reg.
  RelocInfo::Mode rmode_;

  friend class MacroAssembler;
};

// Class MemOperand represents a memory operand in load and store instructions
class MemOperand BASE_EMBEDDED {
 public:
   
  MemOperand(Register base, int offset)
    : base_(base), regoffset_(no_reg), offset_(offset) {
      DCHECK(is_int13(offset_));
  }

  MemOperand(Register base, Register regoffset = g0)
    : base_(base), regoffset_(regoffset), offset_(0) {
  }

  const Register& base() const { return base_; }
  const Register& regoffset() const { return regoffset_; }
  int64_t offset() const { return offset_; }
  bool IsImmediateOffset() const { return regoffset_.is(no_reg); }
  bool IsRegisterOffset() const { return !regoffset_.is(no_reg); }

private:
  Register base_;
  Register regoffset_;
  int offset_;
 
  friend class Assembler;
};



// Argument is an abstraction used to represent an outgoing
// actual argument or an incoming formal parameter, whether
// it resides in memory or in a register, in a manner consistent
// with the SPARC Application Binary Interface, or ABI.  This is
// often referred to as the native or C calling convention.

class Argument BASE_EMBEDDED {
 private:
  int _number;
  bool _is_in;

 public:
#ifdef _LP64
  enum {
    n_register_parameters = 6,          // only 6 registers may contain integer parameters
    n_float_register_parameters = 16    // Can have up to 16 floating registers
  };
#else
  enum {
    n_register_parameters = 6           // only 6 registers may contain integer parameters
  };
#endif

  // creation
  Argument(int number, bool is_in) : _number(number), _is_in(is_in) {}

  int  number() const  { return _number;  }
  bool is_in()  const  { return _is_in;   }
  bool is_out() const  { return !is_in(); }

  Argument successor() const  { return Argument(number() + 1, is_in()); }
  Argument as_in()     const  { return Argument(number(), true ); }
  Argument as_out()    const  { return Argument(number(), false); }

  // locating register-based arguments:
  bool is_register() const { return _number < n_register_parameters; }

#ifdef _LP64
  // locating Floating Point register-based arguments:
  bool is_float_register() const { return _number < n_float_register_parameters; }

  FloatRegister as_float_register() const {
    DCHECK(is_float_register());//, "must be a register argument");
    return as_FloatRegister(( number() *2 ) + 1);
  }
  FloatRegister as_double_register() const {
    DCHECK(is_float_register());//, "must be a register argument");
    return as_FloatRegister(( number() *2 ));
  }
#endif

  Register as_register() const {
    DCHECK(is_register());//, "must be a register argument");
    return is_in() ? as_iRegister(number()) : as_oRegister(number());
  }

  // locating memory-based arguments
  MemOperand as_address() const {
    DCHECK(!is_register());//, "must be a memory argument");
    return address_in_frame();
  }

  // When applied to a register-based argument, give the corresponding address
  // into the 6-word area "into which callee may store register arguments"
  // (This is a different place than the corresponding register-save area location.)
  MemOperand address_in_frame() const;

  // debugging
  const char* name() const;

  friend class Assembler;
};

inline MemOperand temp_address_in_frame(int index) {
    return MemOperand(fp, kStackBias - (index+1) * kWordSize );
}


// Generate a MemOperand for loading a field from an object.
inline MemOperand FieldMemOperand(Register object, int offset) {
  return MemOperand(object, offset - kHeapObjectTag);
}

// Temporal is an abstraction used to represent a temp var,
// whether it resides in memory or in a register.

class Temporary BASE_EMBEDDED {
 private:
  int _number;

  public:
  enum {
    n_register_locals = 8,         // l0-l7
  };

  // creation
  Temporary(int number, bool force_stack = false) : _number(number) {
      if(force_stack)
          _number += n_register_locals;
  }

  int  number() const  { return _number;  }

  Temporary successor() const  { return Temporary(number() + 1); }

  // locating register-based arguments:
  bool is_register() const { return _number < n_register_locals; }

  Register as_register() const {
    DCHECK(is_register());//, "must be a register argument");
    return as_lRegister(number());
  }

  // locating memory-based arguments
  MemOperand as_address() const {
    DCHECK(!is_register());//, "must be a memory argument");
    return address_in_frame();
  }

  // When applied to a register-based argument, give the corresponding address
  // into the 6-word area "into which callee may store register arguments"
  // (This is a different place than the corresponding register-save area location.)
  MemOperand address_in_frame() const {
      return MemOperand(fp, kStackBias - (_number - n_register_locals +1) * kWordSize );
  }

  // debugging
  const char* name() const;

  friend class Assembler;
};




// MacroAssembler implements a collection of frequently used macros.
class MacroAssembler : public Assembler {
public:
  // The isolate parameter can be NULL if the macro assembler should
  // not use isolate-dependent functionality. In this case, it's the
  // responsibility of the caller to never invoke such function on the
  // macro assembler.
  MacroAssembler(Isolate* isolate, byte* buffer, unsigned buffer_size,
                 CodeObjectRequired create_code_object);

  // support for delayed instructions
  MacroAssembler* delayed() { Assembler::delayed();  return this; }
 
  inline void Save(int locals_count = 0);
 
  // Required platform-specific helpers for Label::patch_instructions.
  // They _shadow_ the declarations in AbstractAssembler, which are undefined.
  //void pd_patch_instruction(address branch, address target);

  // sethi Macro handles optimizations and relocations
private:
  void internal_sethi(const Operand& src, Register d, bool ForceRelocatable);
public:
  void sethi(const Operand& src, Register d);
  void patchable_sethi(const Operand& src, Register d);

  // compute the number of instructions for a sethi/set
  static int  insts_for_sethi( intptr_t a, bool worst_case = false );
  static int  worst_case_insts_for_set();

  // set may be either setsw or setuw (high 32 bits may be zero or sign)
private:
  void internal_set(const Operand& src, Register d, bool ForceRelocatable);
  static int insts_for_internal_set(intptr_t value);
public:
  void set(const Operand& src, Register d);
  static int insts_for_set(intptr_t value) { return insts_for_internal_set(value); }

  void patchable_set(const Operand& src, Register d);
  void patchable_set(intptr_t value, Register d);
  void set64(int64_t value, Register d, Register tmp);
  static int insts_for_set64(int64_t value);

  /* 
  void sethi(const Operand& src, Register d);  
  void set(const Operand& src, Register d);
  void set64(int64_t value, Register d, Register tmp);
*/
  // traps as per trap.h (SPARC ABI?)

  void breakpoint_trap();
  void breakpoint_trap(Condition c, CC cc);

  // Double only float instructions
  void faddd(FloatRegister s1, FloatRegister s2, FloatRegister d ) { fadd(FloatRegister::D, s1, s2, d); }
  void fsubd(FloatRegister s1, FloatRegister s2, FloatRegister d ) { fsub(FloatRegister::D, s1, s2, d); }
  void fcmpd(  CC cc, FloatRegister s1, FloatRegister s2) { fcmp(FloatRegister::D, cc, s1, s2); }
  void fcmped(  CC cc, FloatRegister s1, FloatRegister s2) { fcmpe(FloatRegister::D, cc, s1, s2); }
  void fdtox( FloatRegister s, FloatRegister d ) {  ftox(FloatRegister::D, s, d); }
  void fdtoi( FloatRegister s, FloatRegister d ) {  ftoi(FloatRegister::D, s, d); }
  void fxtod( FloatRegister s, FloatRegister d ) { fxtof(FloatRegister::D, s, d); }
  void fitod( FloatRegister s, FloatRegister d ) { fitof(FloatRegister::D, s, d); }
  void fmovd( FloatRegister s, FloatRegister d ) { fmov(FloatRegister::D, s, d); }
  void fmovd( FPUCondition c,  bool floatCC, CC cca, FloatRegister s2, FloatRegister d ) { fmov(FloatRegister::D, c, floatCC, cca, s2, d); }
  void fmovd( RCondition c,  Register s1, FloatRegister s2, FloatRegister d ) { fmov(FloatRegister::D, c, s1, s2, d); }
  void fnegd( FloatRegister s, FloatRegister d ) { fneg(FloatRegister::D, s, d); }
  void fabsd( FloatRegister s, FloatRegister d ) { fabs(FloatRegister::D, s, d); }
  void fmuld(FloatRegister s1, FloatRegister s2, FloatRegister d ) { fmul(FloatRegister::D, s1, s2, d); }
  void fdivd(FloatRegister s1, FloatRegister s2, FloatRegister d ) { fdiv(FloatRegister::D, s1, s2, d); }
  void fxord(FloatRegister s1, FloatRegister s2, FloatRegister d ) { fxor(FloatRegister::D, s1, s2, d); }
  void fsqrtd(FloatRegister s, FloatRegister d ) { fsqrt(FloatRegister::D, s, d); }
  void lddf(Register s1, Register s2, FloatRegister d ) { ldf(FloatRegister::D, s1, s2, d); }
  void lddf(Register s1, int simm13a, FloatRegister d ) { ldf(FloatRegister::D, s1, simm13a, d); }
  void lddfa( Register s1, Register s2, int ia, FloatRegister d ) { ldfa(FloatRegister::D, s1, s2, ia, d); }
  void lddfa( Register s1, int simm13a, FloatRegister d ) { ldf(FloatRegister::D, s1, simm13a, d); }
  void stdf( FloatRegister d, Register s1, Register s2) { stf(FloatRegister::D, d, s1, s2); }
  void stdf( FloatRegister d, Register s1, int simm13a ) { stf(FloatRegister::D, d, s1, simm13a); }
  void stdfa( FloatRegister d, Register s1, Register s2, int ia ) { stfa(FloatRegister::D, d, s1, s2, ia); }
  void stdfa( FloatRegister d, Register s1, int simm13a ) { stfa(FloatRegister::D, d, s1, simm13a); }


  // branches that use right instruction for v8 vs. v9
  inline void br( Condition c, bool a, Predict p, int d );
  inline void br( Condition c, bool a, Predict p, Label* L );

  inline void fb( FPUCondition c, bool a, Predict p, int d );
  inline void fb( FPUCondition c, bool a, Predict p, Label* L );

  // compares register with zero (32 bit) and branches (V9 and V8 instructions)
  void cmp_zero_and_br( Condition c, Register s1, Label* L, bool a = false, Predict p = pn );
  // Compares a pointer register with zero and branches on (not)null.
  // Does a test & branch on 32-bit systems and a register-branch on 64-bit.
  void br_null   ( Register s1, bool a, Predict p, Label* L );
  void br_notnull( Register s1, bool a, Predict p, Label* L );

  //
  // Compare registers and branch with nop in delay slot or cbcond without delay slot.
  //
  // ATTENTION: use these instructions with caution because cbcond instruction
  //            has very short distance: 512 instructions (2Kbyte).

  // Compare integer (32 bit) values (icc only).
  void cmp_and_br_short(Register s1, Register s2, Condition c, Predict p, Label* L);
  void cmp_and_br_short(Register s1, int simm13a, Condition c, Predict p, Label* L);
  // Platform depending version for pointer compare (icc on !LP64 and xcc on LP64).
  void cmp_and_brx_short(Register s1, Register s2, Condition c, Predict p, Label* L);
  void cmp_and_brx_short(Register s1, int simm13a, Condition c, Predict p, Label* L);

  // Short branch version for compares a pointer pwith zero.
  void br_null_short   ( Register s1, Predict p, Label* L );
  void br_notnull_short( Register s1, Predict p, Label* L );

  // unconditional short branch
  void ba_short(Label* L);

  inline void bp( Condition c, bool a, CC cc, Predict p, int d );
  inline void bp( Condition c, bool a, CC cc, Predict p, Label* L );

  // Branch that tests xcc in LP64 and icc in !LP64
  inline void brx( Condition c, bool a, Predict p, int d );
  inline void brx( Condition c, bool a, Predict p, Label* L );

  // unconditional branch
  inline void ba( Label* L );

  // Branch that tests fp condition codes
  inline void fbp( FPUCondition c, bool a, CC cc, Predict p, int d );
  void fbp( FPUCondition c, bool a, CC cc, Predict p, Label* L );

  // pp 297 Synthetic Instructions
  inline void cmp(  Register s1, Register s2 ) { subcc( s1, s2, g0 ); }
  inline void cmp(  Register s1, int simm13a ) { subcc( s1, simm13a, g0 ); }

  inline void jmp( Register s1, Register s2 );
  inline void jmp( Register s1, int simm13a );

  inline void callr( Register s1, Register s2 );
  inline void callr( Register s1, int simm13a );

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

  using Assembler::ldsb;
  using Assembler::ldsh;
  using Assembler::ldsw;
  using Assembler::ldub;
  using Assembler::lduh;
  using Assembler::lduw;
  using Assembler::ldx;
  using Assembler::ldd;
 
  inline void ld(Register s1, Register s2, Register d);
  inline void ld(Register s1, int simm13a, Register d);
 
  // MemOperand based Loads and Stores
  inline void ldsb(const MemOperand& s, Register d);
  inline void ldsh(const MemOperand& s, Register d);
  inline void ldsw(const MemOperand& s, Register d);
  inline void ldub(const MemOperand& s, Register d);
  inline void lduh(const MemOperand& s, Register d);
  inline void lduw(const MemOperand& s, Register d);
  inline void ldx(const MemOperand& s, Register d);
  inline void ld(const MemOperand& s, Register d);
  inline void ldd(const MemOperand& s, Register d);
  inline void lddf(const MemOperand& s, FloatRegister d);

  
  using Assembler::stb;
  using Assembler::sth;
  using Assembler::stw;
  using Assembler::stx;
  using Assembler::std;

  inline void st(Register d, Register s1, Register s2);
  inline void st(Register d, Register s1, int simm13a);
  
  inline void stb(Register d, const MemOperand& s);
  inline void sth(Register d, const MemOperand& s);
  inline void stw(Register d, const MemOperand& s);
  inline void stx(Register d, const MemOperand& s);
  inline void std(Register d, const MemOperand& s);
  inline void st(Register d, const MemOperand& s);
  inline void stdf(FloatRegister d, const MemOperand& s);

  using Assembler::stf;
  inline void stf(FloatRegister::Width w, FloatRegister d, const MemOperand& s);

    // argument pseudos:

  inline void load_argument( Argument& a, Register  d );
  inline void store_argument( Register s, Argument& a );
  inline void store_ptr_argument( Register s, Argument& a );
  inline void store_float_argument( FloatRegister s, Argument& a );
  inline void store_double_argument( FloatRegister s, Argument& a );
  inline void store_long_argument( Register s, Argument& a );

     // local temporary pseudos:

  inline void load_temporary( Temporary& t, Register  d );
  inline void load_ptr_temporary( Temporary& t, Register  d );
  inline void store_temporary( Register s, Temporary& t );
  inline void store_ptr_temporary( Register s, Temporary& t );
  inline void store_long_temporary( Register s, Temporary& t );

  inline void load_frame_offset( int s, Register  d );
  inline void store_frame_offset( Register s, int d );

  // handy macros:

  inline void round_to( Register r, int modulus ) {
    assert_not_delayed();
    inc( r, modulus - 1 );
    and3( r, -modulus, r );
  }

  // --------------------------------------------------

  // Functions for isolating 64 bit loads for LP64
  // ld_ptr will perform ld for 32 bit VM's and ldx for 64 bit VM's
  // st_ptr will perform st for 32 bit VM's and stx for 64 bit VM's
  inline void ld_ptr(Register s1, Register s2, Register d);
  inline void ld_ptr(Register s1, int simm13a, Register d);
  inline void ld_ptr(const MemOperand& s, Register d);
  inline void st_ptr(Register d, Register s1, Register s2);
  inline void st_ptr(Register d, Register s1, int simm13a);
  inline void st_ptr(Register d, const MemOperand& s);

  inline void load_contents(const Operand& src, Register d);
  inline void load_ptr_contents(const Operand& src, Register d);
  inline void store_contents(Register s, const Operand& dst, Register temp);
  inline void store_ptr_contents(Register s, const Operand& dst, Register temp);
  inline void jumpl_to(const Operand& src, Register temp, Register d);
  inline void jump_to(const Operand& src, Register temp);

#ifdef ASSERT
  // ByteSize is only a class when ASSERT is defined, otherwise it's an int.
  inline void ld_ptr(Register s1, ByteSize simm13a, Register d);
  inline void st_ptr(Register d, Register s1, ByteSize simm13a);
#endif

  // ld_long will perform ldd for 32 bit VM's and ldx for 64 bit VM's
  // st_long will perform std for 32 bit VM's and stx for 64 bit VM's
  inline void ld_long(Register s1, Register s2, Register d);
  inline void ld_long(Register s1, int simm13a, Register d);
  inline void ld_long(const MemOperand& s, Register d);
  inline void st_long(Register d, Register s1, Register s2);
  inline void st_long(Register d, Register s1, int simm13a);
  inline void st_long(Register d, const MemOperand& s);

  void InitializeRootRegister() {
    ExternalReference roots_array_start =
        ExternalReference::roots_array_start(isolate());
    set(Operand(roots_array_start), kRootRegister);
  }

  // Generates function and stub prologue code.
  void Prologue(bool code_pre_aging, int locals_count);
  
  // Size of the generated code in bytes
  uint64_t SizeOfGeneratedCode() const {
    DCHECK((pc_ >= buffer_) && (pc_ < (buffer_ + buffer_size_)));
    return pc_ - buffer_;
  }

  // Return the code size generated from label to the current position.
  uint64_t SizeOfCodeGeneratedSince(const Label* label) {
    DCHECK(label->is_bound());
    DCHECK(pc_offset() >= label->pos());
    DCHECK(pc_offset() < buffer_size_);
    return pc_offset() - label->pos();
  }

  // Check the size of the code generated since the given label. This function
  // is used primarily to work around comparisons between signed and unsigned
  // quantities, since V8 uses both.
  // TODO(jbramley): Work out what sign to use for these things and if possible,
  // change things to be consistent.
  void AssertSizeOfCodeGeneratedSince(const Label* label, ptrdiff_t size) {
    DCHECK(size >= 0);
    DCHECK(static_cast<uint64_t>(size) == SizeOfCodeGeneratedSince(label));
  }

  // Return the number of instructions generated from label to the
  // current position.
  uint64_t InstructionsGeneratedSince(const Label* label) {
    return SizeOfCodeGeneratedSince(label) / kInstructionSize;
  }
  
  void Jump(Register target) { UNIMPLEMENTED(); }
  void Jump(Address target, RelocInfo::Mode rmode, Condition cond = always) { UNIMPLEMENTED(); }
  void Jump(Handle<Code> code, RelocInfo::Mode rmode, Condition cond = always) { UNIMPLEMENTED(); }
  void Jump(intptr_t target, RelocInfo::Mode rmode, Condition cond = always) { UNIMPLEMENTED(); }

/*  void Call(Register target) { UNIMPLEMENTED(); }*/
  void Call(Label* target) { UNIMPLEMENTED(); }
  void Call(Address target, RelocInfo::Mode rmode);
  void Call(Handle<Code> code,
            RelocInfo::Mode rmode = RelocInfo::CODE_TARGET,
            TypeFeedbackId ast_id = TypeFeedbackId::None());

  // For every Call variant, there is a matching CallSize function that returns
  // the size (in bytes) of the call sequence.
 /* static int CallSize(Register target) { UNIMPLEMENTED(); }*/
  static int CallSize(Label* target) { UNIMPLEMENTED(); }
  static int CallSize(Address target, RelocInfo::Mode rmode);
  static int CallSize(Handle<Code> code,
                      RelocInfo::Mode rmode = RelocInfo::CODE_TARGET,
                      TypeFeedbackId ast_id = TypeFeedbackId::None()) { UNIMPLEMENTED(); }
  
  static int CallSizeNotPredictableCodeSize(Address target,
                                            RelocInfo::Mode rmode,
                                            Condition condition = always) { UNIMPLEMENTED(); }
  void JumpToJSEntry(Register target) { UNIMPLEMENTED(); }
  void CallJSEntry(Register target) { UNIMPLEMENTED(); }
  void Ret()  { UNIMPLEMENTED(); }
 
  // Emit code to discard a non-negative number of pointer-sized elements
  // from the stack, clobbering only the sp register.
  void Drop(int count) { UNIMPLEMENTED(); }

  void Ret(int drop)  { UNIMPLEMENTED(); }

  // Emit call to the code we are currently generating.
  void CallSelf() {
    Handle<Code> self(reinterpret_cast<Code**>(CodeObject().location()));
    Call(self, RelocInfo::CODE_TARGET);
  }

  // Register move. May do nothing if the registers are identical.
  void Move(Register dst, Handle<Object> value) { UNIMPLEMENTED(); }
  void Move(Register dst, Register src, Condition condition = always) { UNIMPLEMENTED(); }
  void Move(DoubleRegister dst, DoubleRegister src) { UNIMPLEMENTED(); }

  // Load an object from the root table.
  void LoadRoot(Heap::RootListIndex index, Register destination);
  // Store an object to the root table.
  void StoreRoot(Register source, Heap::RootListIndex index);

  void PushRoot(Heap::RootListIndex index) { UNIMPLEMENTED(); }

   void Push(Register src) { UNIMPLEMENTED(); }

  // Push a handle.
  void Push(Handle<Object> handle) { UNIMPLEMENTED(); }
  void Push(Smi* smi) { Push(Handle<Smi>(smi, isolate())); }

  void Pop(Register dst)  { UNIMPLEMENTED(); }
  
  // Verify restrictions about code generated in stubs.
  void set_generating_stub(bool value) { generating_stub_ = value; }
  bool generating_stub() { return generating_stub_; }
  void set_has_frame(bool value) { has_frame_ = value; }
  bool has_frame() { return has_frame_; }
  inline bool AllowThisStubCall(CodeStub* stub);

 // Activation support.
 void EnterFrame(StackFrame::Type type, Register additional_words);
 void EnterFrame(StackFrame::Type type, bool load_constant_pool_pointer_reg);
 void EnterFrame(StackFrame::Type type);
 void LeaveFrame(StackFrame::Type type);

   // Allocates a heap number or jumps to the gc_required label if the young
  // space is full and a scavenge is needed. All registers are clobbered also
  // when control continues at the gc_required label.
  void AllocateHeapNumber(Register result,
                          Register scratch1,
                          Register scratch2,
                          Register heap_number_map,
                          Label* gc_required,
                          TaggingMode tagging_mode = TAG_RESULT,
                          MutableMode mode = IMMUTABLE) { UNIMPLEMENTED(); }

  
  // For a given |object| notify the garbage collector that the slot |address|
  // has been written.  |value| is the object being stored. The value and
  // address registers are clobbered by the operation.
  void RecordWrite(
      Register object,
      Register address,
      Register value,
      RAStatus ra_status,
      SaveFPRegsMode save_fp,
      RememberedSetAction remembered_set_action = EMIT_REMEMBERED_SET,
      SmiCheck smi_check = INLINE_SMI_CHECK,
      PointersToHereCheck pointers_to_here_check_for_value =
          kPointersToHereMaybeInteresting);

  // This is required for compatibility in architecture indepenedant code.
  inline void jmp(Label* L) { UNIMPLEMENTED(); }
  
  // Compare object type for heap object.
  // Always use unsigned comparisons: above and below, not less and greater.
  // Incoming register is heap_object and outgoing register is map.
  // They may be the same register, and may be kScratchRegister.
  void CmpObjectType(Register heap_object, InstanceType type, Register map);

  // Compare instance type for map.
  // Always use unsigned comparisons: above and below, not less and greater.
  void CmpInstanceType(Register map, InstanceType type);

  // -------------------------------------------------------------------------
  // Debugger Support.

  void DebugBreak() { UNIMPLEMENTED(); }

    // ---------------------------------------------------------------------------
  // Debugging

  // Calls Abort(msg) if the condition cond is not satisfied.
  // Use --debug_code to enable.
  void Assert(Condition cond, BailoutReason reason);
  void AssertFastElements(Register elements);

  // Like Assert(), but always enabled.
  void Check(Condition cond, BailoutReason reason);

  // Print a message to stdout and abort execution.
  void Abort(BailoutReason reason);

  // Abort execution if argument is not a JSFunction, enabled via --debug-code.
  void AssertFunction(Register object);

  // ---------------------------------------------------------------------------
  // Exception handling

  // Store a new stack handler and link into stack handler chain.
  void LinkStackHandler(Temporary& link_frame_slot); // is PushStackHandler() in all other Platforms

  // Unlink the stack handler from the stack handler chain.
  // Must preserve the result register.
  void UnlinkStackHandler(Temporary& link_frame_slot); // is PopStackHandler() in all other Platforms  
  // ---------------------------------------------------------------------------
  // Runtime calls

  void CallStub(CodeStub* stub, TypeFeedbackId ast_id = TypeFeedbackId::None());
  void TailCallStub(CodeStub* stub);
  

 // Call a runtime routine.
  void CallRuntime(const Runtime::Function* f, int num_arguments,
                   SaveFPRegsMode save_doubles = kDontSaveFPRegs/*,
                   BranchDelaySlot bd = PROTECT*/);
  void CallRuntimeSaveDoubles(Runtime::FunctionId fid) {
    const Runtime::Function* function = Runtime::FunctionForId(fid);
    CallRuntime(function, function->nargs, kSaveFPRegs);
  }

  // Convenience function: Same as above, but takes the fid instead.
  void CallRuntime(Runtime::FunctionId fid,
                   SaveFPRegsMode save_doubles = kDontSaveFPRegs/*,
                   BranchDelaySlot bd = PROTECT*/) {
    const Runtime::Function* function = Runtime::FunctionForId(fid);
    CallRuntime(function, function->nargs, save_doubles/*, bd*/);
  }

  // Convenience function: Same as above, but takes the fid instead.
  void CallRuntime(Runtime::FunctionId fid, int num_arguments,
                   SaveFPRegsMode save_doubles = kDontSaveFPRegs/*,
                   BranchDelaySlot bd = PROTECT*/) {
    CallRuntime(Runtime::FunctionForId(fid), num_arguments, save_doubles/*, bd*/);
  }

  // Convenience function: call an external reference.
  void CallExternalReference(const ExternalReference& ext, int num_arguments) { UNIMPLEMENTED(); }


 
  // Tail call of a runtime routine (jump).
  // Like JumpToExternalReference, but also takes care of passing the number
  // of parameters.
  void TailCallExternalReference(const ExternalReference& ext,
                                 int num_arguments,
                                 int result_size);
 
  // Convenience function: tail call a runtime routine (jump).
  void TailCallRuntime(Runtime::FunctionId fid);


  Handle<Object> CodeObject() {
    DCHECK(!code_object_.is_null());
    return code_object_;
  }

  // Check if the map of an object is equal to a specified weak map and branch
  // to a specified target if equal. Skip the smi check if not required
  // (object is known to be a heap object)
  void DispatchWeakMap(Register obj, Register scratch1, Register scratch2,
                       Handle<WeakCell> cell, Handle<Code> success,
                       SmiCheckType smi_check_type) { UNIMPLEMENTED(); }

  // If the value is a NaN, canonicalize the value else, do nothing.
  void FPUCanonicalizeNaN(const DoubleRegister dst, const DoubleRegister src) { UNIMPLEMENTED(); }


  // Get value of the weak cell.
  void GetWeakValue(Register value, Handle<WeakCell> cell) { UNIMPLEMENTED(); }

  // Load the value of the weak cell in the value register. Branch to the
  // given miss label is the weak cell was cleared.
  void LoadWeakValue(Register value, Handle<WeakCell> cell, Label* miss) { UNIMPLEMENTED(); }

private:
  // Compute memory operands for safepoint stack slots.
  static int SafepointRegisterStackIndex(int reg_code) { UNIMPLEMENTED(); }
  MemOperand SafepointRegisterSlot(Register reg) { UNIMPLEMENTED(); }
  MemOperand SafepointRegistersAndDoublesSlot(Register reg) { UNIMPLEMENTED(); }

  bool generating_stub_;
  bool has_frame_;
  // This handle will be patched with the code object on installation.
  Handle<Object> code_object_;

  // Needs access to SafepointRegisterStackIndex for compiled frame
  // traversal.
  friend class StandardFrame;
};


class SparcFrameScope {
 public:
  explicit SparcFrameScope(MacroAssembler* masm, StackFrame::Type type, Register additional_words = g0)
      : masm_(masm), type_(type), old_has_frame_(masm->has_frame()) {
    masm->set_has_frame(true);
    if (type != StackFrame::MANUAL && type_ != StackFrame::NONE) {
      masm->EnterFrame(type, additional_words);
    }
  }

  ~SparcFrameScope() {
    if (type_ != StackFrame::MANUAL && type_ != StackFrame::NONE) {
      masm_->LeaveFrame(type_);
    }
    masm_->set_has_frame(old_has_frame_);
  }

  // Normally we generate the leave-frame code when this object goes
  // out of scope.  Sometimes we may need to generate the code somewhere else
  // in addition.  Calling this will achieve that, but the object stays in
  // scope, the MacroAssembler is still marked as being in a frame scope, and
  // the code will be generated again when it goes out of scope.
  void GenerateLeaveFrame() {
    DCHECK(type_ != StackFrame::MANUAL && type_ != StackFrame::NONE);
    masm_->LeaveFrame(type_);
  }

 private:
  MacroAssembler* masm_;
  StackFrame::Type type_;
  bool old_has_frame_;
};



// The code patcher is used to patch (typically) small parts of code e.g. for
// debugging and other types of instrumentation. When using the code patcher
// the exact number of bytes specified must be emitted. It is not legal to emit
// relocation information. If any of these constraints are violated it causes
// an assertion to fail.
class CodePatcher {
 public:
  enum FlushICache {
    FLUSH,
    DONT_FLUSH
  };

  CodePatcher(Isolate* isolate, byte* address, int instructions,
              FlushICache flush_cache = FLUSH);
  ~CodePatcher();

  // Macro assembler to emit code.
  MacroAssembler* masm() { return &masm_; }

  // Emit an instruction directly.
  void emit_int32(int instr);

 private:
  byte* address_;            // The address of the code being patched.
  int size_;                 // Number of bytes of the expected patch size.
  MacroAssembler masm_;      // Macro assembler used to generate the code.
  FlushICache flush_cache_;  // Whether to flush the I cache after patching.
};

#ifdef GENERATED_CODE_COVERAGE
#define CODE_COVERAGE_STRINGIFY(x) #x
#define CODE_COVERAGE_TOSTRING(x) CODE_COVERAGE_STRINGIFY(x)
#define __FILE_LINE__ __FILE__ ":" CODE_COVERAGE_TOSTRING(__LINE__)
#define ACCESS_MASM(masm) masm->stop(__FILE_LINE__); masm->
#else
#define ACCESS_MASM(masm) masm->
#endif

}  // namespace internal
}  // namespace v8

#endif  // V8_SPARC_MACRO_ASSEMBLER_SPARC_H_