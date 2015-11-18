// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include <iostream>

#include "include/libplatform/libplatform.h"
#include "include/v8.h"
#include "src/debug/debug.h"
#include "src/macro-assembler.h"

using namespace v8;
using namespace v8::internal; 

typedef int (*F0)();
typedef int (*F1)(int64_t x);
typedef int (*F2)(int64_t x, int64_t y);
typedef unsigned (*F3)(double x);
typedef uint64_t (*F4)(uint64_t* x, uint64_t* y);
typedef uint64_t (*F5)(uint64_t x);

#define __ assm.

int main(int argc, char* argv[]) {
  // Initialize V8.
  V8::InitializeICU();
  V8::InitializeExternalStartupData(argv[0]);
  Platform* platform = platform::CreateDefaultPlatform();
  V8::InitializePlatform(platform);
  V8::Initialize();

   size_t actual_size;
  // Allocate buffer in executable space.
  byte* buffer = static_cast<byte*>(base::OS::Allocate(1024, &actual_size, true));
  Assembler assm(NULL, buffer, static_cast<int>(actual_size));

  __ retl();
  __ delayed()->nop(); 
 
  CodeDesc desc;
  assm.GetCode(&desc);

  Assembler::FlushICacheWithoutIsolate(buffer, actual_size);
  base::OS::ProtectCode(buffer, actual_size);

 /* int result = */ FUNCTION_CAST<F0>(buffer)();
//  CHECK_EQ(1, result);
  
  V8::Dispose();
  V8::ShutdownPlatform();
  delete platform;
  return 0;
}

#undef __
