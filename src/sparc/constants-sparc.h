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

const unsigned kFixedFrameSize = 176;

// CHECK_NEXT
enum Condition {
  kNoCondition = -1,
  al = 0,
   z = 1,         // Integer Register Zero.
  lez = 2,       // Integer Register Less Than or Equal to Zero .
  lz = 3,        // Integer Register Less Than Zero.
  nz = 5,       //  Integer Register Not Zero.
  gz = 6,       //  Integer Register Greater Than Zero
  gez = 7     //  Integer Register Greater Than or Equal to Zeror
 };

 // CHECK_NEXT
enum FPUCondition {
  kNoFPUCondition = -1,

  F = 0x00,    // False.
  UN = 0x01,   // Unordered.
  EQ = 0x02,   // Equal.
  UEQ = 0x03,  // Unordered or Equal.
  OLT = 0x04,  // Ordered or Less Than, on Mips release < 6.
  LT = 0x04,   // Ordered or Less Than, on Mips release >= 6.
  ULT = 0x05,  // Unordered or Less Than.
  OLE = 0x06,  // Ordered or Less Than or Equal, on Mips release < 6.
  LE = 0x06,   // Ordered or Less Than or Equal, on Mips release >= 6.
  ULE = 0x07,  // Unordered or Less Than or Equal.

  // Following constants are available on Mips release >= 6 only.
  ORD = 0x11,  // Ordered, on Mips release >= 6.
  UNE = 0x12,  // Not equal, on Mips release >= 6.
  NE = 0x13,   // Ordered Greater Than or Less Than. on Mips >= 6 only.
};



}  // namespace internal
}  // namespace v8

#endif  // V8_SPARC_CONSTANTS_SPARC_H_
