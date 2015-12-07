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

// The original source code covered by the above license above has been modified
// significantly by Google Inc.
// Copyright 2014 the V8 project authors. All rights reserved.

#ifndef V8_SPARC_MACRO_ASSEMBLER_SPARC_INL_H_
#define V8_SPARC_MACRO_ASSEMBLER_SPARC_INL_H_

#include "src/sparc/macro-assembler-sparc.h"

namespace v8 {
namespace internal {

    // -----------------------------------------------------------------------------
// Operand and MemOperand.

Operand::Operand(int64_t immediate, RelocInfo::Mode rmode)  {
  rm_ = no_reg;
  imm64_ = immediate;
  rmode_ = rmode;
}


Operand::Operand(const ExternalReference& f)  {
  rm_ = no_reg;
  imm64_ = reinterpret_cast<int64_t>(f.address());
  rmode_ = RelocInfo::EXTERNAL_REFERENCE;
}


Operand::Operand(Smi* value) {
  rm_ = no_reg;
  imm64_ =  reinterpret_cast<intptr_t>(value);
  rmode_ = RelocInfo::NONE32;
}


Operand::Operand(Register rm) {
  rm_ = rm;
}


bool Operand::is_reg() const {
  return rm_.is_valid();
}
      
// Use the right branch for the platform
inline void MacroAssembler::Save(int locals_count) {
    Assembler::save(sp, - (kFixedFrameSize + locals_count * kPointerSize), sp);
}

inline void MacroAssembler::br( Condition c, bool a, Predict p, int d ) {
  Assembler::bp(c, a, icc, p, d);
}

inline void MacroAssembler::br( Condition c, bool a, Predict p, Label* L ) {
  insert_nop_after_cbcond();
  br(c, a, p, branch_offset(L));
}


// Branch that tests either xcc or icc depending on the
// architecture compiled (LP64 or not)
inline void MacroAssembler::brx( Condition c, bool a, Predict p, int d ) {
#ifdef _LP64
    Assembler::bp(c, a, xcc, p, d);
#else
    MacroAssembler::br(c, a, p, d);
#endif
}

inline void MacroAssembler::brx( Condition c, bool a, Predict p, Label* L ) {
  insert_nop_after_cbcond();
  brx(c, a, p, branch_offset(L));
}

void MacroAssembler::ba( Label* L ) {
  br(always, false, pt, L);
}

// Warning: V9 only functions
inline void MacroAssembler::bp( Condition c, bool a, CC cc, Predict p, int d ) {
  Assembler::bp(c, a, cc, p, d);
}

inline void MacroAssembler::bp( Condition c, bool a, CC cc, Predict p, Label* L ) {
  Assembler::bp(c, a, cc, p, L);
}

inline void MacroAssembler::fb( FPUCondition c, bool a, Predict p, int d ) {
  fbp(c, a, fcc0, p, d);
}

inline void MacroAssembler::fb( FPUCondition c, bool a, Predict p, Label* L ) {
  insert_nop_after_cbcond();
  fb(c, a, p, branch_offset(L));
}

inline void MacroAssembler::fbp( FPUCondition c, bool a, CC cc, Predict p, int d ) {
  Assembler::fbp(c, a, cc, p, d);
}

inline void MacroAssembler::fbp( FPUCondition c, bool a, CC cc, Predict p, Label* L ) {
  Assembler::fbp(c, a, cc, p, L);
}



// Use the right loads/stores for the platform
inline void MacroAssembler::ld_ptr( Register s1, Register s2, Register d ) {
#ifdef _LP64
  Assembler::ldx(s1, s2, d);
#else
             ld( s1, s2, d);
#endif
}

inline void MacroAssembler::ld_ptr( Register s1, int simm13a, Register d ) {
#ifdef _LP64
  Assembler::ldx(s1, simm13a, d);
#else
             ld( s1, simm13a, d);
#endif
}


inline void MacroAssembler::ld_ptr(const MemOperand& s, Register d) {
#ifdef _LP64
  ldx(s, d);
#else
  ld( s, d);
#endif
}

inline void MacroAssembler::st_ptr( Register d, Register s1, Register s2 ) {
#ifdef _LP64
  Assembler::stx(d, s1, s2);
#else
             st( d, s1, s2);
#endif
}

inline void MacroAssembler::st_ptr( Register d, Register s1, int simm13a ) {
#ifdef _LP64
  Assembler::stx(d, s1, simm13a);
#else
             st( d, s1, simm13a);
#endif
}

inline void MacroAssembler::st_ptr(Register d, const MemOperand& s) {
#ifdef _LP64
  stx(d, s);
#else
  st( d, s);
#endif
}

// Use the right loads/stores for the platform
inline void MacroAssembler::ld_long( Register s1, Register s2, Register d ) {
#ifdef _LP64
  Assembler::ldx(s1, s2, d);
#else
  Assembler::ldd(s1, s2, d);
#endif
}

inline void MacroAssembler::ld_long( Register s1, int simm13a, Register d ) {
#ifdef _LP64
  Assembler::ldx(s1, simm13a, d);
#else
  Assembler::ldd(s1, simm13a, d);
#endif
}


inline void MacroAssembler::ld_long(const MemOperand& s, Register d) {
#ifdef _LP64
  ldx(s, d);
#else
  ldd(s, d);
#endif
}

inline void MacroAssembler::st_long( Register d, Register s1, Register s2 ) {
#ifdef _LP64
  Assembler::stx(d, s1, s2);
#else
  Assembler::std(d, s1, s2);
#endif
}

inline void MacroAssembler::st_long( Register d, Register s1, int simm13a ) {
#ifdef _LP64
  Assembler::stx(d, s1, simm13a);
#else
  Assembler::std(d, s1, simm13a);
#endif
}


inline void MacroAssembler::st_long( Register d, const MemOperand& s) {
#ifdef _LP64
  stx(d, s);
#else
  std(d, s);
#endif
}


inline void MacroAssembler::load_argument( Argument& a, Register  d ) {
  if (a.is_register())
    mov(a.as_register(), d);
  else
    ld (a.as_address(),  d);
}

inline void MacroAssembler::store_argument( Register s, Argument& a ) {
  if (a.is_register())
    mov(s, a.as_register());
  else
    st_ptr (s, a.as_address());         // ABI says everything is right justified.
}

inline void MacroAssembler::store_ptr_argument( Register s, Argument& a ) {
  if (a.is_register())
    mov(s, a.as_register());
  else
    st_ptr (s, a.as_address());
}


#ifdef _LP64
inline void MacroAssembler::store_float_argument( FloatRegister s, Argument& a ) {
  if (a.is_float_register())
// V9 ABI has F1, F3, F5 are used to pass instead of O0, O1, O2
    fmov(FloatRegister::S, s, a.as_float_register() );
  else {
    // Floats are stored in the high half of the stack entry
    // The low half is undefined per the ABI.
    MemOperand a_address = a.as_address();
    DCHECK(a_address.IsImmediateOffset());
    MemOperand d(a_address.base(), a_address.offset() + sizeof(float));
    stf(FloatRegister::S, s, d);      
  }
}

inline void MacroAssembler::store_double_argument( FloatRegister s, Argument& a ) {
  if (a.is_float_register())
// V9 ABI has D0, D2, D4 are used to pass instead of O0, O1, O2
    fmov(FloatRegister::D, s, a.as_double_register() );
  else
    stf(FloatRegister::D, s, a.as_address());
}

inline void MacroAssembler::store_long_argument( Register s, Argument& a ) {
  if (a.is_register())
    mov(s, a.as_register());
  else
    stx(s, a.as_address());
}
#endif

inline void MacroAssembler::jmp( Register s1, Register s2 ) { jmpl( s1, s2, g0 ); }
inline void MacroAssembler::jmp( Register s1, int simm13a ) { jmpl( s1, simm13a, g0); }

inline void MacroAssembler::callr( Register s1, Register s2 ) { jmpl( s1, s2, o7 ); }
inline void MacroAssembler::callr( Register s1, int simm13a ) { jmpl( s1, simm13a, o7); }

inline void MacroAssembler::clrb( Register s1, Register s2) { stb( g0, s1, s2 ); }
inline void MacroAssembler::clrh( Register s1, Register s2) { sth( g0, s1, s2 ); }
inline void MacroAssembler::clr(  Register s1, Register s2) { stw( g0, s1, s2 ); }
inline void MacroAssembler::clrx( Register s1, Register s2) { stx( g0, s1, s2 ); }

inline void MacroAssembler::clrb( Register s1, int simm13a) { stb( g0, s1, simm13a); }
inline void MacroAssembler::clrh( Register s1, int simm13a) { sth( g0, s1, simm13a); }
inline void MacroAssembler::clr(  Register s1, int simm13a) { stw( g0, s1, simm13a); }
inline void MacroAssembler::clrx( Register s1, int simm13a) { stx( g0, s1, simm13a); }


#ifdef _LP64
// Make all 32 bit loads signed so 64 bit registers maintain proper sign
inline void MacroAssembler::ld(  Register s1, Register s2, Register d)      { ldsw( s1, s2, d); }
inline void MacroAssembler::ld(  Register s1, int simm13a, Register d)      { ldsw( s1, simm13a, d); }
#else
inline void MacroAssembler::ld(  Register s1, Register s2, Register d)      { lduw( s1, s2, d); }
inline void MacroAssembler::ld(  Register s1, int simm13a, Register d)      { lduw( s1, simm13a, d); }
#endif

inline void MacroAssembler::ld(  const MemOperand& s, Register d) {
  if (s.IsRegisterOffset()) { ld(  s.base(), s.regoffset(),         d); }
  else               {                          ld(  s.base(), s.offset(), d); }
}

inline void MacroAssembler::ldsb(const MemOperand& s, Register d) {
  if (s.IsRegisterOffset()) { ldsb(s.base(), s.regoffset(),         d); }
  else               {                          ldsb(s.base(), s.offset(), d); }
}
inline void MacroAssembler::ldsh(const MemOperand& s, Register d) {
  if (s.IsRegisterOffset()) { ldsh(s.base(), s.regoffset(),         d); }
  else               {                          ldsh(s.base(), s.offset(), d); }
}
inline void MacroAssembler::ldsw(const MemOperand& s, Register d) {
  if (s.IsRegisterOffset()) { ldsw(s.base(), s.regoffset(),         d); }
  else               {                          ldsw(s.base(), s.offset(), d); }
}
inline void MacroAssembler::ldub(const MemOperand& s, Register d) {
  if (s.IsRegisterOffset()) { ldub(s.base(), s.regoffset(),         d); }
  else               {                          ldub(s.base(), s.offset(), d); }
}
inline void MacroAssembler::lduh(const MemOperand& s, Register d) {
  if (s.IsRegisterOffset()) { lduh(s.base(), s.regoffset(),         d); }
  else               {                          lduh(s.base(), s.offset(), d); }
}
inline void MacroAssembler::lduw(const MemOperand& s, Register d) {
  if (s.IsRegisterOffset()) { lduw(s.base(), s.regoffset(),         d); }
  else               {                          lduw(s.base(), s.offset(), d); }
}
inline void MacroAssembler::ldd( const MemOperand& s, Register d) {
  if (s.IsRegisterOffset()) { ldd( s.base(), s.regoffset(),         d); }
  else               {                          ldd( s.base(), s.offset(), d); }
}
inline void MacroAssembler::ldx( const MemOperand& s, Register d) {
  if (s.IsRegisterOffset()) { ldx( s.base(), s.regoffset(),         d); }
  else               {                          ldx( s.base(), s.offset(), d); }
}


inline void MacroAssembler::st(Register d, Register s1, Register s2)      { stw(d, s1, s2); }
inline void MacroAssembler::st(Register d, Register s1, int simm13a)      { stw(d, s1, simm13a); }

inline void MacroAssembler::st(Register d, const MemOperand& s) {
  if (s.IsRegisterOffset()) { st( d, s.base(), s.regoffset()        ); }
  else               {                          st( d, s.base(), s.offset()); }
}

inline void MacroAssembler::stb(Register d, const MemOperand& s) {
  if (s.IsRegisterOffset()) { stb(d, s.base(), s.regoffset()        ); }
  else               {                          stb(d, s.base(), s.offset()); }
}
inline void MacroAssembler::sth(Register d, const MemOperand& s) {
  if (s.IsRegisterOffset()) { sth(d, s.base(), s.regoffset()        ); }
  else               {                          sth(d, s.base(), s.offset()); }
}
inline void MacroAssembler::stw(Register d, const MemOperand& s) {
  if (s.IsRegisterOffset()) { stw(d, s.base(), s.regoffset()        ); }
  else               {                          stw(d, s.base(), s.offset()); }
}
inline void MacroAssembler::std(Register d, const MemOperand& s) {
  if (s.IsRegisterOffset()) { std(d, s.base(), s.regoffset()        ); }
  else               {                          std(d, s.base(), s.offset()); }
}
inline void MacroAssembler::stx(Register d, const MemOperand& s) {
  if (s.IsRegisterOffset()) { stx(d, s.base(), s.regoffset()        ); }
  else               {                          stx(d, s.base(), s.offset()); }
}

inline void MacroAssembler::stf(FloatRegister::Width w, FloatRegister d, const MemOperand& s) {
  if (s.IsRegisterOffset()) { stf(w, d, s.base(), s.regoffset()        ); }
  else               {                          stf(w, d, s.base(), s.offset()); }
}


}  // namespace internal
}  // namespace v8

#endif  // V8_SPARC_MACRO_ASSEMBLER_SPARC_INL_H_
