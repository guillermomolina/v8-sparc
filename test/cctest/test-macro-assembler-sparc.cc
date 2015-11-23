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

// Test the SPARC assembler by compiling some simple functions into
// a buffer and executing them.  These tests do not initialize the
// V8 library, create a context, or use any V8 objects.

typedef int (*F0)();
typedef int (*F1)(int64_t x);
typedef int (*F2)(int64_t x, int64_t y);
typedef unsigned (*F3)(double x);
typedef uint64_t (*F4)(uint64_t* x, uint64_t* y);
typedef uint64_t (*F5)(uint64_t x);

#define __ masm.


TEST(MacroAssemblerSPARCReturnOperation) {
  CcTest::InitializeVM();
  // Allocate an executable page of memory.
  size_t actual_size;
  byte* buffer = static_cast<byte*>(v8::base::OS::Allocate(
      Assembler::kMinimalBufferSize, &actual_size, true));
  CHECK(buffer);
  MacroAssembler masm(CcTest::i_isolate(), buffer, static_cast<int>(actual_size));

  // Assemble a simple function that copies argument 2 and returns it.
  __ retl(); 
  __ delayed()->mov( o1, o0); 

  CodeDesc desc;
  masm.GetCode(&desc);
  // Call the function from C++.
  int result =  FUNCTION_CAST<F2>(buffer)(3, 2);
  CHECK_EQ(2, result);
}


TEST(MacroAssemblerSPARCStackOperations) {
  CcTest::InitializeVM();
  // Allocate an executable page of memory.
  size_t actual_size;
  byte* buffer = static_cast<byte*>(v8::base::OS::Allocate(
      Assembler::kMinimalBufferSize, &actual_size, true));
  CHECK(buffer);
  MacroAssembler masm(CcTest::i_isolate(), buffer, static_cast<int>(actual_size));

  // Assemble a simple function that copies argument 2 and returns it.
  __ save(sp, -176, sp); // Make room in stack
  __ mov(i1, i0); 
  __ ret(); 
  __ delayed()->restore(); // free the stack


  CodeDesc desc;
  masm.GetCode(&desc);
  // Call the function from C++.
  int result =  FUNCTION_CAST<F2>(buffer)(3, 2);
  CHECK_EQ(2, result);
}


TEST(MacroAssemblerSPARCArithmeticOperations) {
  CcTest::InitializeVM();
  // Allocate an executable page of memory.
  size_t actual_size;
  byte* buffer = static_cast<byte*>(v8::base::OS::Allocate(
      Assembler::kMinimalBufferSize, &actual_size, true));
  CHECK(buffer);
  MacroAssembler masm(CcTest::i_isolate(), buffer, static_cast<int>(actual_size));

  // Assemble a simple function that adds arguments returning the sum.
  __ retl();
  __ delayed()->add(o0, o1, o0); 

  CodeDesc desc;
  masm.GetCode(&desc);
  // Call the function from C++.
  int result =  FUNCTION_CAST<F2>(buffer)(3, 2);
  CHECK_EQ(5, result);
}


TEST(MacroAssemblerSPARCCmpOperation) {
  CcTest::InitializeVM();
  // Allocate an executable page of memory.
  size_t actual_size;
  byte* buffer = static_cast<byte*>(v8::base::OS::Allocate(
      Assembler::kMinimalBufferSize, &actual_size, true));
  CHECK(buffer);
  MacroAssembler masm(CcTest::i_isolate(), buffer, static_cast<int>(actual_size));

  // Assemble an instruction that compares two arguments, return 0 if equal or 1 if not equal
  __ cmp(o0, o1);
  Label done;
  __ bp( equal, false,  xcc,  pt, &done );
  __ delayed()->clr(o0);
  __ inc(o0);
  __ bind(&done);
  __ retl();
  __ delayed()->nop(); 
 
  CodeDesc desc;
  masm.GetCode(&desc);
  // Call the function from C++.
  int result = FUNCTION_CAST<F2>(buffer)(0x123456789, 0x987654321);
  CHECK_EQ(1, result);
  result = FUNCTION_CAST<F2>(buffer)(0x123456789, 0x123456789);
  CHECK_EQ(0, result);
}


TEST(MacroAssemblerSPARCBranchForwardBackward) {
  CcTest::InitializeVM();
  // Allocate an executable page of memory.
  size_t actual_size;
  byte* buffer = static_cast<byte*>(v8::base::OS::Allocate(
      Assembler::kMinimalBufferSize, &actual_size, true));
  CHECK(buffer);
  MacroAssembler masm(CcTest::i_isolate(), buffer, static_cast<int>(actual_size));

  Label label1;
  __ bp( always, false,  xcc,  pt, &label1 );
  __ delayed()->clr(o0);
  
  Label label2;
  __ bind(&label2);
  Label done;
  __ bp( always, false,  xcc,  pt, &done );
  __ delayed()->inc(o0); 
  
  __ bind(&label1);
  __ bp( always, false,  xcc,  pt, &label2 );
  __ delayed()->inc(o0); 
 
  __ bind(&done);
  __ retl();
  __ delayed()->inc(o0); 
 
  CodeDesc desc;
  masm.GetCode(&desc);
  // Call the function from C++.
 int result = FUNCTION_CAST<F0>(buffer)();
 CHECK_EQ(3, result);
}


TEST(MacroAssemblerSPARCFAdd) {
  CcTest::InitializeVM();
  // Allocate an executable page of memory.
  size_t actual_size;
  byte* buffer = static_cast<byte*>(v8::base::OS::Allocate(
      Assembler::kMinimalBufferSize, &actual_size, true));
  CHECK(buffer);
  MacroAssembler masm(CcTest::i_isolate(), buffer, static_cast<int>(actual_size));

  __ retl();
  __ delayed()->fadd(FloatRegister::D, f0, f2, f0); 
 
  CodeDesc desc;
  masm.GetCode(&desc);
  // Call the function from C++.
 double result = FUNCTION_CAST<F6>(buffer)(123.34, 23134.34);
 CHECK_EQ(23257.68, result);
}


TEST(MacroAssemblerSPARCImmediate8) {
  CcTest::InitializeVM();
  // Allocate an executable page of memory.
  size_t actual_size;
  byte* buffer = static_cast<byte*>(v8::base::OS::Allocate(
      Assembler::kMinimalBufferSize, &actual_size, true));
  CHECK(buffer);
  MacroAssembler masm(CcTest::i_isolate(), buffer, static_cast<int>(actual_size));

  __ set64(0x56, o0, g1);
  __ retl();
  __ delayed()->nop(); 
 
  CodeDesc desc;
  masm.GetCode(&desc);
  // Call the function from C++.
 int result = FUNCTION_CAST<F0>(buffer)();
 CHECK_EQ(0x56, result);
}


TEST(MacroAssemblerSPARCImmediate16) {
  CcTest::InitializeVM();
  // Allocate an executable page of memory.
  size_t actual_size;
  byte* buffer = static_cast<byte*>(v8::base::OS::Allocate(
      Assembler::kMinimalBufferSize, &actual_size, true));
  CHECK(buffer);
  MacroAssembler masm(CcTest::i_isolate(), buffer, static_cast<int>(actual_size));

  __ set64(0x3456, o0, g1);
  __ retl();
  __ delayed()->nop(); 
 
  CodeDesc desc;
  masm.GetCode(&desc);
  // Call the function from C++.
 int result = FUNCTION_CAST<F0>(buffer)();
 CHECK_EQ(0x3456, result);
}


TEST(MacroAssemblerSPARCImmediate32) {
  CcTest::InitializeVM();
  // Allocate an executable page of memory.
  size_t actual_size;
  byte* buffer = static_cast<byte*>(v8::base::OS::Allocate(
      Assembler::kMinimalBufferSize, &actual_size, true));
  CHECK(buffer);
  MacroAssembler masm(CcTest::i_isolate(), buffer, static_cast<int>(actual_size));

  __ set64(0xF0123456, o0, g1);
  __ retl();
  __ delayed()->nop(); 
 
  CodeDesc desc;
  masm.GetCode(&desc);
  // Call the function from C++.
 int result = FUNCTION_CAST<F0>(buffer)();
 CHECK_EQ(0xF0123456, result);
}


TEST(MacroAssemblerSPARCImmediate64) {
  CcTest::InitializeVM();
  // Allocate an executable page of memory.
  size_t actual_size;
  byte* buffer = static_cast<byte*>(v8::base::OS::Allocate(
      Assembler::kMinimalBufferSize, &actual_size, true));
  CHECK(buffer);
  MacroAssembler masm(CcTest::i_isolate(), buffer, static_cast<int>(actual_size));

  __ set64(0x789ABCDEF0123456, o0, g1);
  __ retl();
  __ delayed()->nop(); 
 
  CodeDesc desc;
  masm.GetCode(&desc);
  // Call the function from C++.
 int result = FUNCTION_CAST<F0>(buffer)();
 CHECK_EQ(0x789ABCDEF0123456, result);
}

#undef __

