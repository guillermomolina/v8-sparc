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

void Assembler::CheckBuffer() {
  if (buffer_space() <= kGap) {
    GrowBuffer();
  }
}

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


Address Assembler::target_address_from_return_address(Address pc) {
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

}  // namespace internal
}  // namespace v8

#endif  // V8_SPARC_ASSEMBLER_SPARC_INL_H_
