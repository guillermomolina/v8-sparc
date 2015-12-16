// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// Flags: --expose-debug-as debug --allow-natives-syntax

var Debug = debug.Debug;
var expected =
    ["debugger;", "var x = y;", "var b = 2;", "Debug.setListener(null);"];
var log = [];

function listener(event, exec_state, event_data, data) {
  if (event != Debug.DebugEvent.Break) return;
  try {
    log.push(exec_state.frame(0).sourceLineText().trimLeft());
    exec_state.prepareStep(Debug.StepAction.StepNext, 1);
  } catch (e) {
    %AbortJS(e + "\n" + e.stack);
  }
}

Debug.setListener(listener);

function f() {
  var a = 1;
  debugger;
  var x = y;
  print(x);
}

try {
  %Call(f, {});
} catch (e) {
  var b = 2;
}

Debug.setListener(null);

assertEquals(expected, log);
