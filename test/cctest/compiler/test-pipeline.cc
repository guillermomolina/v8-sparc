// Copyright 2013 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// TODO(jochen): Remove this after the setting is turned on globally.
#define V8_IMMINENT_DEPRECATION_WARNINGS

#include "src/compiler.h"
#include "src/compiler/pipeline.h"
#include "src/handles.h"
#include "src/parser.h"
#include "test/cctest/cctest.h"

using namespace v8::internal;
using namespace v8::internal::compiler;

static void RunPipeline(Zone* zone, const char* source) {
  Handle<JSFunction> function = Handle<JSFunction>::cast(v8::Utils::OpenHandle(
      *v8::Local<v8::Function>::Cast(CompileRun(source))));
  ParseInfo parse_info(zone, function);
  CHECK(Compiler::ParseAndAnalyze(&parse_info));
  CompilationInfo info(&parse_info);
  info.SetOptimizing(BailoutId::None(), Handle<Code>(function->code()));

  Pipeline pipeline(&info);
  Handle<Code> code = pipeline.GenerateCode();
  CHECK(!code.is_null());
}


TEST(PipelineTyped) {
  HandleAndZoneScope handles;
  FLAG_turbo_types = true;
  RunPipeline(handles.main_zone(), "(function(a,b) { return a + b; })");
}


TEST(PipelineGeneric) {
  HandleAndZoneScope handles;
  FLAG_turbo_types = false;
  RunPipeline(handles.main_zone(), "(function(a,b) { return a + b; })");
}
