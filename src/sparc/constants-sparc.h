// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_SPARC_CONSTANTS_SPARC_H_
#define V8_SPARC_CONSTANTS_SPARC_H_

#include <stdint.h>

#include "src/base/logging.h"
#include "src/base/macros.h"
#include "src/globals.h"

namespace v8 {
namespace internal {

const unsigned kInstructionSize = 4;
const unsigned kInstructionSizeLog2 = 2;

// CHECK_NEXT
enum Condition {
  kNoCondition = -1,
   z = 1,         // Integer Register Zero.
  lez = 2,       // Integer Register Less Than or Equal to Zero .
  lz = 3,        // Integer Register Less Than Zero.
  nz = 5,       //  Integer Register Not Zero.
  gz = 6,       //  Integer Register Greater Than Zero
  gez = 7     //  Integer Register Greater Than or Equal to Zeror
 };

}  // namespace internal
}  // namespace v8

#endif  // V8_SPARC_CONSTANTS_SPARC_H_
