// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include <iostream>

#include "include/libplatform/libplatform.h"
#include "include/v8.h"
#include "src/debug/debug.h"

using namespace v8; 

class ArrayBufferAllocator : public v8::ArrayBuffer::Allocator {
 public:
  virtual void* Allocate(size_t length) {
    void* data = AllocateUninitialized(length);
    return data == NULL ? data : memset(data, 0, length);
  }
  virtual void* AllocateUninitialized(size_t length) { return malloc(length); }
  virtual void Free(void* data, size_t) { free(data); }
};


int main(int argc, char* argv[]) {
  // Initialize V8.
  V8::InitializeICU();
  V8::InitializeExternalStartupData(argv[0]);
  Platform* platform = platform::CreateDefaultPlatform();
  V8::InitializePlatform(platform);
  V8::Initialize();

  // Create a new Isolate and make it the current one.
  ArrayBufferAllocator allocator;
  Isolate::CreateParams create_params;
  create_params.array_buffer_allocator = &allocator;
  Isolate* isolate = Isolate::New(create_params);
  {
    Isolate::Scope isolate_scope(isolate);

    // Create a stack-allocated handle scope.
    HandleScope handle_scope(isolate);

    // Create a new context.
    Local<Context> context = Context::New(isolate);

    // Enter the context for compiling and running the hello world script.
    Context::Scope context_scope(context);

    // Create a string containing the JavaScript source code.
    Local<String> source =
        String::NewFromUtf8(isolate, "'Hello' + ', World!'",
                            NewStringType::kNormal).ToLocalChecked();

    // Compile the source code.
    Local<Script> script = Script::Compile(context, source).ToLocalChecked();

    // Run the script to get the result.
    Local<Value> result = script->Run(context).ToLocalChecked();

    // Convert the result to an UTF8 string and print it.
    String::Utf8Value utf8(result);
    printf("%s\n", *utf8);
  }
  V8::Dispose();
  V8::ShutdownPlatform();
  delete platform;
  return 0;
}
/*

#include <iostream>

#include "include/libplatform/libplatform.h"
#include "include/v8.h"
#include "src/debug/debug.h"
#include "src/macro-assembler.h"
#include "test/cctest/cctest.h"


typedef int (*F2)(int64_t x, int64_t y);

using namespace v8::internal; 

#define __ masm.

int main(int argc, char* argv[]) {
  // Initialize V8.
  v8::V8::InitializeICU();
  v8::V8::InitializeExternalStartupData(argv[0]);
  v8::Platform* platform = v8::platform::CreateDefaultPlatform();
  v8::V8::InitializePlatform(platform);
  v8::V8::Initialize();

  // Allocate an executable page of memory.
  size_t actual_size;
  byte* buffer = static_cast<byte*>(v8::base::OS::Allocate(
      Assembler::kMinimalBufferSize, &actual_size, true));
  CHECK(buffer);
  MacroAssembler masm(NULL, buffer, static_cast<int>(actual_size), CodeObjectRequired::kNo);

  // Assemble a simple function that copies argument 2 and returns it.
  int locals_count = 18;
  int Root=1;
  __ Save(locals_count); // Make room in stack
   __ mov(Root, g2);
  __ add(fp, kStackBias - kPointerSize, g1);
  __ add(fp, kStackBias - (locals_count + 1) * kPointerSize, g3);
  __ stx(g2, MemOperand(g1));
  Label loop;
  __ bind(&loop);
  __ add(g1, -kPointerSize, g1);
  __ cmp(g1, g3);
  __ brx(notEqual, true, pt, &loop);
  __ delayed()->stx(g2, MemOperand(g1));;
  __ ret(); 
  __ delayed()->restore(); // free the stack
  CodeDesc desc;
  masm.GetCode(&desc);
  // Call the function from C++.
  int result =  FUNCTION_CAST<F2>(buffer)(3, 2);
 CHECK_EQ(2, result);

  v8::V8::Dispose();
  v8::V8::ShutdownPlatform();
  delete platform;
  return 0;
}
  
#undef __
*/