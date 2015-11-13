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

#ifndef V8_SPARC_ASSEMBLER_SPARC_INL_H_
#define V8_SPARC_ASSEMBLER_SPARC_INL_H_

#include "src/sparc/assembler-sparc.h"

#include "src/assembler.h"
#include "src/debug/debug.h"


namespace v8 {
namespace internal {

 bool CpuFeatures::SupportsCrankshaft() { return false; }   
    
// CHECK_NEXT
static const int kNoCodeAgeSequenceLength = 5 * kInstructionSize;


void RelocInfo::apply(intptr_t delta) {
    UNIMPLEMENTED();
}

Object* RelocInfo::target_object() {
    UNIMPLEMENTED();
}

Handle<Object> RelocInfo::target_object_handle(Assembler* origin) {
    UNIMPLEMENTED();
}

void RelocInfo::set_target_object(Object* target,
                                  WriteBarrierMode write_barrier_mode,
                                  ICacheFlushMode icache_flush_mode) {
    UNIMPLEMENTED();
}

Address RelocInfo::target_external_reference() {
    UNIMPLEMENTED();
}

Address RelocInfo::target_internal_reference() {
    UNIMPLEMENTED();
}

Address RelocInfo::target_internal_reference_address() {
    UNIMPLEMENTED();
}

Address RelocInfo::target_runtime_entry(Assembler* origin) {
    UNIMPLEMENTED();
}


Address RelocInfo::target_address_address() {
    UNIMPLEMENTED();
}

void RelocInfo::set_target_runtime_entry(Address target,
                                         WriteBarrierMode write_barrier_mode,
                                         ICacheFlushMode icache_flush_mode) {
    UNIMPLEMENTED();
}

Address RelocInfo::target_address() {
  DCHECK(IsCodeTarget(rmode_) || IsRuntimeEntry(rmode_));
  return Assembler::target_address_at(pc_, host_);
}

Handle<Cell> RelocInfo::target_cell_handle() {
    UNIMPLEMENTED();
}

Cell* RelocInfo::target_cell() {
    UNIMPLEMENTED();
}

void RelocInfo::set_target_cell(Cell* cell,
                                WriteBarrierMode write_barrier_mode,
                                ICacheFlushMode icache_flush_mode) {
    UNIMPLEMENTED();
}

Handle<Object> RelocInfo::code_age_stub_handle(Assembler* origin) {
    UNIMPLEMENTED();
}


Code* RelocInfo::code_age_stub() {
    UNIMPLEMENTED();
}


void RelocInfo::set_code_age_stub(Code* stub,
                                  ICacheFlushMode icache_flush_mode) {
    UNIMPLEMENTED();
}


Address RelocInfo::debug_call_address() {
    UNIMPLEMENTED();
}


void RelocInfo::set_debug_call_address(Address target) {
    UNIMPLEMENTED();
}

bool RelocInfo::IsPatchedDebugBreakSlotSequence() {
    UNIMPLEMENTED();
}

void RelocInfo::set_target_address(Address target,
                                   WriteBarrierMode write_barrier_mode,
                                   ICacheFlushMode icache_flush_mode) {
    UNIMPLEMENTED();
}

int RelocInfo::target_address_size() {
    UNIMPLEMENTED();
}

void RelocInfo::WipeOut() {
    UNIMPLEMENTED();
}


void RelocInfo::Visit(Isolate* isolate, ObjectVisitor* visitor) {
  RelocInfo::Mode mode = rmode();
  if (mode == RelocInfo::EMBEDDED_OBJECT) {
    visitor->VisitEmbeddedPointer(this);
  } else if (RelocInfo::IsCodeTarget(mode)) {
    visitor->VisitCodeTarget(this);
  } else if (mode == RelocInfo::CELL) {
    visitor->VisitCell(this);
  } else if (mode == RelocInfo::EXTERNAL_REFERENCE) {
    visitor->VisitExternalReference(this);
  } else if (mode == RelocInfo::INTERNAL_REFERENCE ||
             mode == RelocInfo::INTERNAL_REFERENCE_ENCODED) {
    visitor->VisitInternalReference(this);
  } else if (RelocInfo::IsCodeAgeSequence(mode)) {
    visitor->VisitCodeAgeSequence(this);
  } else if (RelocInfo::IsDebugBreakSlot(mode) &&
             IsPatchedDebugBreakSlotSequence()) {
    visitor->VisitDebugTarget(this);
  } else if (RelocInfo::IsRuntimeEntry(mode)) {
    visitor->VisitRuntimeEntry(this);
  }
}


template<typename StaticVisitor>
void RelocInfo::Visit(Heap* heap) {
  RelocInfo::Mode mode = rmode();
  if (mode == RelocInfo::EMBEDDED_OBJECT) {
    StaticVisitor::VisitEmbeddedPointer(heap, this);
  } else if (RelocInfo::IsCodeTarget(mode)) {
    StaticVisitor::VisitCodeTarget(heap, this);
  } else if (mode == RelocInfo::CELL) {
    StaticVisitor::VisitCell(heap, this);
  } else if (mode == RelocInfo::EXTERNAL_REFERENCE) {
    StaticVisitor::VisitExternalReference(this);
  } else if (mode == RelocInfo::INTERNAL_REFERENCE ||
             mode == RelocInfo::INTERNAL_REFERENCE_ENCODED) {
    StaticVisitor::VisitInternalReference(this);
  } else if (RelocInfo::IsCodeAgeSequence(mode)) {
    StaticVisitor::VisitCodeAgeSequence(heap, this);
  } else if (RelocInfo::IsDebugBreakSlot(mode) &&
             IsPatchedDebugBreakSlotSequence()) {
    StaticVisitor::VisitDebugTarget(heap, this);
  } else if (RelocInfo::IsRuntimeEntry(mode)) {
    StaticVisitor::VisitRuntimeEntry(this);
  }
}


Address RelocInfo::constant_pool_entry_address() {
  UNIMPLEMENTED();
}


 void Assembler::CheckBuffer() {
  if (buffer_space() <= kGap) {
    GrowBuffer();
  }
}

Address Assembler::target_address_from_return_address(Address pc) {
   UNIMPLEMENTED();
}


inline void Assembler::check_delay() {
# ifdef CHECK_DELAY
  guarantee( delay_state != at_delay_slot, "must say delayed() when filling delay slot");
  delay_state = no_delay;
# endif
}

inline void Assembler::emit_int32(int instruction) {
    check_delay();
    STATIC_ASSERT(sizeof(*pc_) == 1);
     DCHECK((pc_ + sizeof(instruction)) <= (buffer_ + buffer_size_));

    *pc_ = instruction;
    pc_ += kInstructionSize;
//    CheckBuffer();
}

inline void Assembler::emit_data(void const * data, unsigned size) {
    DCHECK(sizeof(*pc_) == 1);
    DCHECK((pc_ + size) <= (buffer_ + buffer_size_));

    // TODO(all): Somehow register we have some data here. Then we can
    // disassemble it correctly.
    memcpy(pc_, data, size);
    pc_ += size;
    CheckBuffer();
}

inline void Assembler::add(Register s1, Register s2, Register d ) { 
    emit_int32( op(arith_op) | rd(d) | op3(add_op3) | rs1(s1) | rs2(s2) );
}

inline void Assembler::add(Register s1, int simm13a, Register d ) { 
    emit_int32( op(arith_op) | rd(d) | op3(add_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) );
}


inline void Assembler::bpr( RCondition c, bool a, Predict p, Register s1, int disp16 ) {    
    v9_only(); 
    insert_nop_after_cbcond(); 
    cti();  
    emit_int32( op(branch_op) | annul(a) | cond(c) | op2(bpr_op2) | wdisp16(disp16) | predict(p) | rs1(s1) );  
    has_delay_slot(); 
}

inline void Assembler::bpr( RCondition c, bool a, Predict p, Register s1, Label* L) { insert_nop_after_cbcond(); bpr( c, a, p, s1, target(L)); }

inline void Assembler::fb( Condition c, bool a, int disp22 ) {
    v9_dep();  
    insert_nop_after_cbcond(); 
    cti();  
    emit_int32( op(branch_op) | annul(a) | cond(c) | op2(fb_op2) | wdisp(disp22, 22) );  
    has_delay_slot(); 
}

inline void Assembler::fb( Condition c, bool a, Label* L ) { insert_nop_after_cbcond(); fb(c, a, target(L)); }


inline void Assembler::fbp( Condition c, bool a, CC cc, Predict p, int disp19 ) { 
    v9_only(); 
    insert_nop_after_cbcond(); 
    cti();  
    emit_int32( op(branch_op) | annul(a) | cond(c) | op2(fbp_op2) | branchcc(cc) | predict(p) | wdisp(disp19, 19) );  
    has_delay_slot(); 
}

inline void Assembler::fbp( Condition c, bool a, CC cc, Predict p, Label* L ) { insert_nop_after_cbcond(); fbp(c, a, cc, p, target(L)); }


inline void Assembler::br( Condition c, bool a, int disp22 ) {
    v9_dep(); 
    insert_nop_after_cbcond(); 
    cti();   
    emit_int32( op(branch_op) | annul(a) | cond(c) | op2(br_op2) | wdisp(disp22, 22) );  
    has_delay_slot(); 
}

inline void Assembler::br( Condition c, bool a, Label* L ) { insert_nop_after_cbcond(); br(c, a, target(L)); }


inline void Assembler::bp( Condition c, bool a, CC cc, Predict p,int disp19 ) { 
    v9_only();  
    insert_nop_after_cbcond(); 
    cti();  
    emit_int32( op(branch_op) | annul(a) | cond(c) | op2(bp_op2) | branchcc(cc) | predict(p) | wdisp(disp19, 19));  
    has_delay_slot(); 
}

inline void Assembler::bp( Condition c, bool a, CC cc, Predict p, Label* L ) { insert_nop_after_cbcond(); bp(c, a, cc, p, target(L)); }


// compare and branch
inline void Assembler::cbcond(Condition c, CC cc, Register s1, Register s2, Label* L) { 
    cti(); 
    no_cbcond_before();  
    emit_int32(op(branch_op) | cond_cbcond(c) | op2(bpr_op2) | branchcc(cc) | wdisp10( target(L) ) | rs1(s1) | rs2(s2)); 
}

inline void Assembler::cbcond(Condition c, CC cc, Register s1, int simm5, Label* L)   { 
    cti();  
    no_cbcond_before();  
    emit_int32(op(branch_op) | cond_cbcond(c) | op2(bpr_op2) | branchcc(cc) | wdisp10( target(L) ) | rs1(s1) | immed(true) | simm(simm5, 5)); 
}

inline void Assembler::call( int disp30 ) {
    insert_nop_after_cbcond();
    cti();  
    emit_int32( op(call_op) | wdisp( disp30, 30));  
    has_delay_slot(); 
}
inline void Assembler::call( Label* L ) { insert_nop_after_cbcond(); call( target(L)); }

inline void Assembler::flush( Register s1, Register s2) { emit_int32( op(arith_op) | op3(flush_op3) | rs1(s1) | rs2(s2)); }
inline void Assembler::flush( Register s1, int simm13a) { emit_int32( op(arith_op) | op3(flush_op3) | rs1(s1) | immed(true) | simm(simm13a, 13)); }

inline void Assembler::jmpl( Register s1, Register s2, Register d ) { insert_nop_after_cbcond(); cti();  emit_int32( op(arith_op) | rd(d) | op3(jmpl_op3) | rs1(s1) | rs2(s2));  has_delay_slot(); }
inline void Assembler::jmpl( Register s1, int simm13a, Register d ) { insert_nop_after_cbcond(); cti();  emit_int32( op(arith_op) | rd(d) | op3(jmpl_op3) | rs1(s1) | immed(true) | simm(simm13a, 13) );  has_delay_slot(); }

inline void Assembler::ldf(FloatRegister::Width w, Register s1, Register s2, FloatRegister d) { emit_int32( op(ldst_op) | fd(d, w) | alt_op3(ldf_op3, w) | rs1(s1) | rs2(s2) ); }
inline void Assembler::ldf(FloatRegister::Width w, Register s1, int simm13a, FloatRegister d) { emit_int32( op(ldst_op) | fd(d, w) | alt_op3(ldf_op3, w) | rs1(s1) | immed(true) | simm(simm13a, 13) ); }

inline void Assembler::ldxfsr( Register s1, Register s2) { v9_only();  emit_int32( op(ldst_op) | rd(g1)    | op3(ldfsr_op3) | rs1(s1) | rs2(s2) ); }
inline void Assembler::ldxfsr( Register s1, int simm13a) { v9_only();  emit_int32( op(ldst_op) | rd(g1)    | op3(ldfsr_op3) | rs1(s1) | immed(true) | simm(simm13a, 13)); }

inline void Assembler::ldsb(  Register s1, Register s2, Register d) { emit_int32( op(ldst_op) | rd(d) | op3(ldsb_op3) | rs1(s1) | rs2(s2) ); }
inline void Assembler::ldsb(  Register s1, int simm13a, Register d) { emit_int32( op(ldst_op) | rd(d) | op3(ldsb_op3) | rs1(s1) | immed(true) | simm(simm13a, 13)); }

inline void Assembler::ldsh(  Register s1, Register s2, Register d) { emit_int32( op(ldst_op) | rd(d) | op3(ldsh_op3) | rs1(s1) | rs2(s2) ); }
inline void Assembler::ldsh(  Register s1, int simm13a, Register d) { emit_int32( op(ldst_op) | rd(d) | op3(ldsh_op3) | rs1(s1) | immed(true) | simm(simm13a, 13)); }
inline void Assembler::ldsw(  Register s1, Register s2, Register d) { emit_int32( op(ldst_op) | rd(d) | op3(ldsw_op3) | rs1(s1) | rs2(s2) ); }
inline void Assembler::ldsw(  Register s1, int simm13a, Register d) { emit_int32( op(ldst_op) | rd(d) | op3(ldsw_op3) | rs1(s1) | immed(true) | simm(simm13a, 13)); }
inline void Assembler::ldub(  Register s1, Register s2, Register d) { emit_int32( op(ldst_op) | rd(d) | op3(ldub_op3) | rs1(s1) | rs2(s2) ); }
inline void Assembler::ldub(  Register s1, int simm13a, Register d) { emit_int32( op(ldst_op) | rd(d) | op3(ldub_op3) | rs1(s1) | immed(true) | simm(simm13a, 13)); }
inline void Assembler::lduh(  Register s1, Register s2, Register d) { emit_int32( op(ldst_op) | rd(d) | op3(lduh_op3) | rs1(s1) | rs2(s2) ); }
inline void Assembler::lduh(  Register s1, int simm13a, Register d) { emit_int32( op(ldst_op) | rd(d) | op3(lduh_op3) | rs1(s1) | immed(true) | simm(simm13a, 13)); }
inline void Assembler::lduw(  Register s1, Register s2, Register d) { emit_int32( op(ldst_op) | rd(d) | op3(lduw_op3) | rs1(s1) | rs2(s2) ); }
inline void Assembler::lduw(  Register s1, int simm13a, Register d) { emit_int32( op(ldst_op) | rd(d) | op3(lduw_op3) | rs1(s1) | immed(true) | simm(simm13a, 13)); }

inline void Assembler::ldx(   Register s1, Register s2, Register d) { v9_only();  emit_int32( op(ldst_op) | rd(d) | op3(ldx_op3) | rs1(s1) | rs2(s2) ); }
inline void Assembler::ldx(   Register s1, int simm13a, Register d) { v9_only();  emit_int32( op(ldst_op) | rd(d) | op3(ldx_op3) | rs1(s1) | immed(true) | simm(simm13a, 13)); }
inline void Assembler::ldd(   Register s1, Register s2, Register d) { v9_dep(); DCHECK(d.is_even()); emit_int32( op(ldst_op) | rd(d) | op3(ldd_op3) | rs1(s1) | rs2(s2) ); }
inline void Assembler::ldd(   Register s1, int simm13a, Register d) { v9_dep(); DCHECK(d.is_even()); emit_int32( op(ldst_op) | rd(d) | op3(ldd_op3) | rs1(s1) | immed(true) | simm(simm13a, 13)); }

inline void Assembler::rett( Register s1, Register s2 ) { cti();  emit_int32( op(arith_op) | op3(rett_op3) | rs1(s1) | rs2(s2));  has_delay_slot(); }
inline void Assembler::rett( Register s1, int simm13a) { cti();  emit_int32( op(arith_op) | op3(rett_op3) | rs1(s1) | immed(true) | simm(simm13a, 13));  has_delay_slot(); }

inline void Assembler::sethi( int imm22a, Register d ) { emit_int32( op(branch_op) | rd(d) | op2(sethi_op2) | hi22(imm22a) ); }

  // pp 222

inline void Assembler::stf(    FloatRegister::Width w, FloatRegister d, Register s1, Register s2) { emit_int32( op(ldst_op) | fd(d, w) | alt_op3(stf_op3, w) | rs1(s1) | rs2(s2) ); }
inline void Assembler::stf(    FloatRegister::Width w, FloatRegister d, Register s1, int simm13a) { emit_int32( op(ldst_op) | fd(d, w) | alt_op3(stf_op3, w) | rs1(s1) | immed(true) | simm(simm13a, 13)); }

inline void Assembler::stxfsr( Register s1, Register s2) { v9_only();  emit_int32( op(ldst_op) | rd(g1)    | op3(stfsr_op3) | rs1(s1) | rs2(s2) ); }
inline void Assembler::stxfsr( Register s1, int simm13a) { v9_only();  emit_int32( op(ldst_op) | rd(g1)    | op3(stfsr_op3) | rs1(s1) | immed(true) | simm(simm13a, 13)); }

  // p 226

inline void Assembler::stb(  Register d, Register s1, Register s2) { emit_int32( op(ldst_op) | rd(d) | op3(stb_op3) | rs1(s1) | rs2(s2) ); }
inline void Assembler::stb(  Register d, Register s1, int simm13a) { emit_int32( op(ldst_op) | rd(d) | op3(stb_op3) | rs1(s1) | immed(true) | simm(simm13a, 13)); }
inline void Assembler::sth(  Register d, Register s1, Register s2) { emit_int32( op(ldst_op) | rd(d) | op3(sth_op3) | rs1(s1) | rs2(s2) ); }
inline void Assembler::sth(  Register d, Register s1, int simm13a) { emit_int32( op(ldst_op) | rd(d) | op3(sth_op3) | rs1(s1) | immed(true) | simm(simm13a, 13)); }
inline void Assembler::stw(  Register d, Register s1, Register s2) { emit_int32( op(ldst_op) | rd(d) | op3(stw_op3) | rs1(s1) | rs2(s2) ); }
inline void Assembler::stw(  Register d, Register s1, int simm13a) { emit_int32( op(ldst_op) | rd(d) | op3(stw_op3) | rs1(s1) | immed(true) | simm(simm13a, 13)); }


inline void Assembler::stx(  Register d, Register s1, Register s2) { v9_only();  emit_int32( op(ldst_op) | rd(d) | op3(stx_op3) | rs1(s1) | rs2(s2) ); }
inline void Assembler::stx(  Register d, Register s1, int simm13a) { v9_only();  emit_int32( op(ldst_op) | rd(d) | op3(stx_op3) | rs1(s1) | immed(true) | simm(simm13a, 13)); }
inline void Assembler::std(  Register d, Register s1, Register s2) { v9_dep(); DCHECK(d.is_even()); emit_int32( op(ldst_op) | rd(d) | op3(std_op3) | rs1(s1) | rs2(s2) ); }
inline void Assembler::std(  Register d, Register s1, int simm13a) { v9_dep(); DCHECK(d.is_even()); emit_int32( op(ldst_op) | rd(d) | op3(std_op3) | rs1(s1) | immed(true) | simm(simm13a, 13)); }

// pp 231

inline void Assembler::swap(    Register s1, Register s2, Register d) { v9_dep();  emit_int32( op(ldst_op) | rd(d) | op3(swap_op3) | rs1(s1) | rs2(s2) ); }
inline void Assembler::swap(    Register s1, int simm13a, Register d) { v9_dep();  emit_int32( op(ldst_op) | rd(d) | op3(swap_op3) | rs1(s1) | immed(true) | simm(simm13a, 13)); }



}  // namespace internal
}  // namespace v8

#endif  // V8_SPARC_ASSEMBLER_SPARC_INL_H_
