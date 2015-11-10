// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_REGEXP_SPARC_REGEXP_MACRO_ASSEMBLER_SPARC_H_
#define V8_REGEXP_SPARC_REGEXP_MACRO_ASSEMBLER_SPARC_H_

#include "src/macro-assembler.h"
#include "src/sparc/assembler-sparc.h"
#include "src/sparc/frames-sparc.h"
#include "src/regexp/regexp-macro-assembler.h"

namespace v8 {
namespace internal {


#ifndef V8_INTERPRETED_REGEXP
class RegExpMacroAssemblerSPARC : public NativeRegExpMacroAssembler {
 public:
  };

#endif  // V8_INTERPRETED_REGEXP
}  // namespace internal
}  // namespace v8

#endif  // V8_REGEXP_SPARC_REGEXP_MACRO_ASSEMBLER_SPARC_H_
