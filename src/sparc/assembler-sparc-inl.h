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

// CHECK_NEXT
static const int kNoCodeAgeSequenceLength = 5 * kInstructionSize;

void Assembler::CheckBuffer() {
  if (buffer_space() <= kGap) {
    GrowBuffer();
  }
}


Object* RelocInfo::target_object() {
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

Address RelocInfo::target_address() {
    UNIMPLEMENTED();
}

Cell* RelocInfo::target_cell() {
    UNIMPLEMENTED();
}


Code* RelocInfo::code_age_stub() {
    UNIMPLEMENTED();
}

}  // namespace internal
}  // namespace v8

#endif  // V8_SPARC_ASSEMBLER_SPARC_INL_H_
