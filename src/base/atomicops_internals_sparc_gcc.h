// Copyright 2012 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// This file is an internal atomic implementation, use atomicops.h instead.
//

#ifndef V8_BASE_ATOMICOPS_INTERNALS_SPARC_H_
#define V8_BASE_ATOMICOPS_INTERNALS_SPARC_H_

#include "src/base/logging.h"

namespace v8 {
namespace base {

// Atomically execute:
//      result = *ptr;
//      if (*ptr == old_value)
//        *ptr = new_value;
//      return result;
//
// I.e., replace "*ptr" with "new_value" if "*ptr" used to be "old_value".
// Always return the old value of "*ptr"
//
// This routine implies no memory barriers.
inline Atomic32 NoBarrier_CompareAndSwap(volatile Atomic32* ptr,
                                         Atomic32 old_value,
                                         Atomic32 new_value) {
    UNIMPLEMENTED();
}

// Atomically store new_value into *ptr, returning the previous value held in
// *ptr.  This routine implies no memory barriers.
inline Atomic32 NoBarrier_AtomicExchange(volatile Atomic32* ptr,
                                         Atomic32 new_value) {
    UNIMPLEMENTED();
}

// Atomically increment *ptr by "increment".  Returns the new value of
// *ptr with the increment applied.  This routine implies no memory barriers.
inline Atomic32 NoBarrier_AtomicIncrement(volatile Atomic32* ptr,
                                          Atomic32 increment) {
    UNIMPLEMENTED();
}

inline Atomic32 Barrier_AtomicIncrement(volatile Atomic32* ptr,
                                        Atomic32 increment) {
    UNIMPLEMENTED();
}

// "Acquire" operations
// ensure that no later memory access can be reordered ahead of the operation.
// "Release" operations ensure that no previous memory access can be reordered
// after the operation.  "Barrier" operations have both "Acquire" and "Release"
// semantics.   A MemoryBarrier() has "Barrier" semantics, but does no memory
// access.
inline Atomic32 Acquire_CompareAndSwap(volatile Atomic32* ptr,
                                       Atomic32 old_value,
                                       Atomic32 new_value) {
    UNIMPLEMENTED();
}

inline Atomic32 Release_CompareAndSwap(volatile Atomic32* ptr,
                                       Atomic32 old_value,
                                       Atomic32 new_value) {
    UNIMPLEMENTED();
}

inline void NoBarrier_Store(volatile Atomic8* ptr, Atomic8 value) {
    UNIMPLEMENTED();
}

inline void NoBarrier_Store(volatile Atomic32* ptr, Atomic32 value) {
    UNIMPLEMENTED();
}

inline void MemoryBarrier() {
    UNIMPLEMENTED();
}

inline void Acquire_Store(volatile Atomic32* ptr, Atomic32 value) {
    UNIMPLEMENTED();
}

inline void Release_Store(volatile Atomic32* ptr, Atomic32 value) {
    UNIMPLEMENTED();
}

inline Atomic8 NoBarrier_Load(volatile const Atomic8* ptr) {
    UNIMPLEMENTED();
}

inline Atomic32 NoBarrier_Load(volatile const Atomic32* ptr) {
    UNIMPLEMENTED();
}

inline Atomic32 Acquire_Load(volatile const Atomic32* ptr) {
    UNIMPLEMENTED();
}

inline Atomic32 Release_Load(volatile const Atomic32* ptr) {
    UNIMPLEMENTED();
}


// 64-bit versions of the atomic ops.

inline Atomic64 NoBarrier_CompareAndSwap(volatile Atomic64* ptr,
                                         Atomic64 old_value,
                                         Atomic64 new_value) {
    UNIMPLEMENTED();
}

// Atomically store new_value into *ptr, returning the previous value held in
// *ptr.  This routine implies no memory barriers.
inline Atomic64 NoBarrier_AtomicExchange(volatile Atomic64* ptr,
                                         Atomic64 new_value) {
    UNIMPLEMENTED();
}

// Atomically increment *ptr by "increment".  Returns the new value of
// *ptr with the increment applied.  This routine implies no memory barriers.
inline Atomic64 NoBarrier_AtomicIncrement(volatile Atomic64* ptr,
                                          Atomic64 increment) {
    UNIMPLEMENTED();
}

inline Atomic64 Barrier_AtomicIncrement(volatile Atomic64* ptr,
                                        Atomic64 increment) {
    UNIMPLEMENTED();
}

// "Acquire" operations
// ensure that no later memory access can be reordered ahead of the operation.
// "Release" operations ensure that no previous memory access can be reordered
// after the operation.  "Barrier" operations have both "Acquire" and "Release"
// semantics.   A MemoryBarrier() has "Barrier" semantics, but does no memory
// access.
inline Atomic64 Acquire_CompareAndSwap(volatile Atomic64* ptr,
                                       Atomic64 old_value,
                                       Atomic64 new_value) {
    UNIMPLEMENTED();
}

inline Atomic64 Release_CompareAndSwap(volatile Atomic64* ptr,
                                       Atomic64 old_value,
                                       Atomic64 new_value) {
    UNIMPLEMENTED();
}

inline void NoBarrier_Store(volatile Atomic64* ptr, Atomic64 value) {
    UNIMPLEMENTED();
}

inline void Acquire_Store(volatile Atomic64* ptr, Atomic64 value) {
    UNIMPLEMENTED();
}

inline void Release_Store(volatile Atomic64* ptr, Atomic64 value) {
    UNIMPLEMENTED();
}

inline Atomic64 NoBarrier_Load(volatile const Atomic64* ptr) {
    UNIMPLEMENTED();
}

inline Atomic64 Acquire_Load(volatile const Atomic64* ptr) {
    UNIMPLEMENTED();
}

inline Atomic64 Release_Load(volatile const Atomic64* ptr) {
    UNIMPLEMENTED();
}

}  // namespace base
}  // namespace v8

#endif  // V8_BASE_ATOMICOPS_INTERNALS_SPARC_GCC_H_
