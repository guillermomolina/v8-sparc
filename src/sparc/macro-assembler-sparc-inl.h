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

inline void MacroAssembler::clrb( Register s1, Register s2) { stb( g0, s1, s2 ); }
inline void MacroAssembler::clrh( Register s1, Register s2) { sth( g0, s1, s2 ); }
inline void MacroAssembler::clr(  Register s1, Register s2) { stw( g0, s1, s2 ); }
inline void MacroAssembler::clrx( Register s1, Register s2) { stx( g0, s1, s2 ); }

inline void MacroAssembler::clrb( Register s1, int simm13a) { stb( g0, s1, simm13a); }
inline void MacroAssembler::clrh( Register s1, int simm13a) { sth( g0, s1, simm13a); }
inline void MacroAssembler::clr(  Register s1, int simm13a) { stw( g0, s1, simm13a); }
inline void MacroAssembler::clrx( Register s1, int simm13a) { stx( g0, s1, simm13a); }

}  // namespace internal
}  // namespace v8

#endif  // V8_SPARC_MACRO_ASSEMBLER_SPARC_INL_H_
