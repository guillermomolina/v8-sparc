// Copyright 2013 the V8 project authors. All rights reserved.
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials provided
//       with the distribution.
//     * Neither the name of Google Inc. nor the names of its
//       contributors may be used to endorse or promote products derived
//       from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <cmath>
#include <limits>

#include "src/v8.h"

#include "src/sparc/simulator-sparc.h"
#include "src/base/utils/random-number-generator.h"
#include "src/macro-assembler.h"
#include "test/cctest/cctest.h"

using namespace v8::internal;

#define __ masm.


TEST(AssemblerIa326) {
 /* CcTest::InitializeVM();

  Isolate* isolate = reinterpret_cast<Isolate*>(CcTest::isolate());
  HandleScope scope(isolate);
  v8::internal::byte buffer[256];
  Assembler assm(isolate, buffer, sizeof buffer);

  __ movsd(xmm0, Operand(esp, 1 * kPointerSize));
  __ movsd(xmm1, Operand(esp, 3 * kPointerSize));
  __ addsd(xmm0, xmm1);
  __ mulsd(xmm0, xmm1);
  __ subsd(xmm0, xmm1);
  __ divsd(xmm0, xmm1);
  // Copy xmm0 to st(0) using eight bytes of stack.
  __ sub(esp, Immediate(8));
  __ movsd(Operand(esp, 0), xmm0);
  __ fld_d(Operand(esp, 0));
  __ add(esp, Immediate(8));
  __ ret(0);

  CodeDesc desc;
  assm.GetCode(&desc);
  Handle<Code> code = isolate->factory()->NewCode(
      desc, Code::ComputeFlags(Code::STUB), Handle<Code>());
#ifdef OBJECT_PRINT
  OFStream os(stdout);
  code->Print(os);
#endif
  F5 f = FUNCTION_CAST<F5>(code->entry());
  double res = f(2.2, 1.1);
  ::printf("f() = %f\n", res);
  CHECK(2.29 < res && res < 2.31);*/
}
