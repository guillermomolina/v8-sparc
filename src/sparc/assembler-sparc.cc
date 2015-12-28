// Copyright (c) 1994-2006 Sun Microsystems Inc.
// All Rights Reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//
// - Redistributions of source code must retain the above copyright notice,
// this list of conditions and the following disclaimer.
//
// - Redistribution in binary form must reproduce the above copyright
// notice, this list of conditions and the following disclaimer in the
// documentation and/or other materials provided with the
// distribution.
//
// - Neither the name of Sun Microsystems or the names of contributors may
// be used to endorse or promote products derived from this software without
// specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
// FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
// COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
// STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
// OF THE POSSIBILITY OF SUCH DAMAGE.

// The original source code covered by the above license above has been
// modified significantly by Google Inc.
// Copyright 2014 the V8 project authors. All rights reserved.

#include "src/sparc/assembler-sparc.h"

#if V8_TARGET_ARCH_SPARC

#include "src/base/bits.h"
#include "src/base/cpu.h"
#include "src/macro-assembler.h"
#include "src/sparc/assembler-sparc-inl.h"

namespace v8 {
namespace internal {
   
 // Get the CPU features enabled by the build.
static unsigned CpuFeaturesImpliedByCompiler() {
  unsigned answer = 0;
  return answer;
}


void CpuFeatures::ProbeImpl(bool cross_compile) {
  supported_ |= CpuFeaturesImpliedByCompiler();
//  cache_line_size_ = 128;

  // Only use statically determined features for cross compile (snapshot).
  if (cross_compile) return;

// Detect whether frim instruction is supported (POWER5+)
// For now we will just check for processors we know do not
// support it
#ifndef USE_SIMULATOR
  // Probe for additional features at runtime.
  base::CPU cpu;
  if (cpu.has_cbcond()) supported_ |= 1u << CBCOND;
  if (cpu.has_aes()) supported_ |= 1u << AES;
  if (cpu.has_sha1()) supported_ |= 1u << SHA1;
  if (cpu.has_sha256()) supported_ |= 1u << SHA256;
  if (cpu.has_sha512()) supported_ |= 1u << SHA512;
  if (cpu.has_crc32c()) supported_ |= 1u << CRC32C;
  if (cpu.has_vis1()) supported_ |= 1u << VIS1;
  if (cpu.has_vis2()) supported_ |= 1u << VIS2;
  if (cpu.has_vis3()) supported_ |= 1u << VIS3;
#else  // Simulator
#endif
}


void CpuFeatures::PrintTarget() { }
void CpuFeatures::PrintFeatures() { }
   
 // -----------------------------------------------------------------------------
// Implementation of RelocInfo.

const int RelocInfo::kApplyMask = RelocInfo::kCodeTargetMask |
                                  1 << RelocInfo::INTERNAL_REFERENCE |
                                  1 << RelocInfo::INTERNAL_REFERENCE_ENCODED;

bool RelocInfo::IsCodedSpecially() {
     WARNING("UNIMPLEMENTED");
     return false;
}


bool RelocInfo::IsInConstantPool() {
     WARNING("UNIMPLEMENTED");    
     return false;
}

Assembler::Assembler(Isolate* isolate, void* buffer, int buffer_size)
    : AssemblerBase(isolate, buffer, buffer_size),
       recorded_ast_id_(TypeFeedbackId::None()),
      positions_recorder_(this) {
  reloc_info_writer.Reposition(buffer_ + buffer_size_, pc_);
  last_bound_pos_ = 0;
#ifdef CHECK_DELAY
    delay_state = no_delay;
#endif  
}

// Fetch the value from the FIXED_SEQUENCE sethi/or
Address Assembler::target_address_at(Address pc) {
  int64_t addr = 0;
  Instr instr = instr_at(pc); // pc => sethi
  int i = 1;  
  CHECK(inv_op(instr) == branch_op && inv_op2(instr) == sethi_op2); 
  addr += static_cast<int64_t>(inv_hi22(instr));  
#ifdef _LP64
  instr = instr_at(pc + i * kInstructionSize); // pc + 1 => or3(d, msb32 & 0x3ff, d)
  i++; // i = 2
  CHECK(inv_op(instr) == arith_op && inv_op3(instr) == or_op3); 
  addr += static_cast<int64_t>(inv_simm13(instr)); // top 3 bits should be 0, should check??

  instr = instr_at(pc + i * kInstructionSize); // pc + 2  => sllx(d, 12, d)
  i++; // i = 3 
  CHECK(inv_op(instr) == arith_op && inv_op3(instr) == sllx_op3 && inv_u_field(instr, 5, 0) == 12);
  addr <<= 12;  

  instr = instr_at(pc + i * kInstructionSize); // pc + 3 => or3(d, (lsb32 >> 20) & 0xfff, d)
  i++; // i = 4
  CHECK(inv_op(instr) == arith_op && inv_op3(instr) == or_op3); 
  addr += static_cast<int64_t>(inv_simm13(instr)); // top 1 bit should be 0, should check??
 
  instr = instr_at(pc + i * kInstructionSize); // pc + 4 => sllx(d, 10, d)
  i++; // i = 5
  CHECK(inv_op(instr) == arith_op && inv_op3(instr) == sllx_op3 && inv_u_field(instr, 5, 0) == 10);
  addr <<= 10;  
 
  instr = instr_at(pc + i * kInstructionSize); // pc + 5 =>  or3(d, (lsb32 >> 10) & 0x3ff, d)
  i++; // i = 6
  CHECK(inv_op(instr) == arith_op && inv_op3(instr) == or_op3); 
  addr += static_cast<int64_t>(inv_simm13(instr)); // top 3 bits should be 0, should check??
 
  instr = instr_at(pc + i * kInstructionSize); // pc + 6 => sllx(d, 10, d)
  i++; // i = 7
  CHECK(inv_op(instr) == arith_op && inv_op3(instr) == sllx_op3 && inv_u_field(instr, 5, 0) == 10); 
  addr <<= 10;  
#endif   
  instr = instr_at(pc + i * kInstructionSize); // last instruction, 32bits i=1, 64bits i=7 => add(d, low10(value), d)
  i++;
  CHECK(i == kInstructionsPerPachableSet);
  CHECK(is_part_of_patchable_set(instr)); 
  addr += static_cast<int64_t>(low10(instr));

  return reinterpret_cast<Address>(addr);
}



// On SPARC32, a target address is stored in a 2-instruction sequence:
//  0  sethi(value, d);
//  1  add(d, low10(value), d);
//
// On SPARC64, a target address is stored in a 8-instruction sequence:
//  0  sethi(msb32, d);  // msb 22-bits
//  1  or3(d, msb32 & 0x3ff, d);                   // msb 32-bits are now in lsb 32
//  2  sllx(d, 12, d);                           // Make room for next 12 bits
//  3  or3(d, (lsb32 >> 20) & 0xfff, d);         // Or in next 12
//  4  sllx(d, 10, d);                // Make room for last 10 bits
//  5  or3(d, (lsb32 >> 10) & 0x3ff, d);         // Or in next 10
//  6  sllx(d, 10, d);                  // Shift leaving disp field 0'd
//  7  add(d, low10(value), d);//
//
// Instruction 1 on 32bits and 7 on 64 can any of
//  
//
//
// Defined in MacroAssembler::set()
// Patching the address must replace all the instructions,
// and flush the i-cache.
//
void Assembler::set_target_address_at(Isolate* isolate, Address pc,
                                      Address target,
                                      ICacheFlushMode icache_flush_mode)  {  
  int i = kInstructionsPerPachableSet - 1;  
  uint64_t addr = reinterpret_cast<uint64_t>(target);    
  
  Instr instr = instr_at(pc + i * kInstructionSize); // last instruction, 32bits i=1, 64bits i=7 => add(d, low10(value), d)
  CHECK(is_part_of_patchable_set(instr)); 
  instr &= ~((1 << 10) - 1);
  instr |= low10(addr);
  instr_at_put(pc + i * kInstructionSize, instr);
  i--;
  
#ifdef _LP64
  instr = instr_at(pc + i * kInstructionSize); // pc + 6 => sllx(d, 10, d)
  CHECK(inv_op(instr) == arith_op && inv_op3(instr) == sllx_op3 && inv_u_field(instr, 5, 0) == 10); 
  addr >>= 10;
   i--; // i = 5
  
  instr = instr_at(pc + i * kInstructionSize); // pc + 5 =>  or3(d, (lsb32 >> 10) & 0x3ff, d)
  CHECK(inv_op(instr) == arith_op && inv_op3(instr) == or_op3); 
  instr &= ~((1 << 10) - 1);
  instr |= low10(addr);
  instr_at_put(pc + i * kInstructionSize, instr);
   i--; // i = 4
  
  instr = instr_at(pc + i * kInstructionSize); // pc + 4 => sllx(d, 10, d)
  CHECK(inv_op(instr) == arith_op && inv_op3(instr) == sllx_op3 && inv_u_field(instr, 5, 0) == 10);
  addr >>= 10;
  i--; // i = 3
 
  instr = instr_at(pc + i * kInstructionSize); // pc + 3 => or3(d, (lsb32 >> 20) & 0xfff, d)
  CHECK(inv_op(instr) == arith_op && inv_op3(instr) == or_op3); 
  instr &= ~((1 << 12) - 1);
  instr |= low12(addr);
  instr_at_put(pc + i * kInstructionSize, instr);
   i--; // i = 2
  
  instr = instr_at(pc + i * kInstructionSize); // pc + 2 => sllx(d, 12, d)
  CHECK(inv_op(instr) == arith_op && inv_op3(instr) == sllx_op3 && inv_u_field(instr, 5, 0) == 12);
  addr >>= 12; 
  i--; // i = 1

  instr = instr_at(pc + i * kInstructionSize); // pc + 1 => or3(d, msb32 & 0x3ff, d)
  CHECK(inv_op(instr) == arith_op && inv_op3(instr) == or_op3); 
  instr &= ~((1 << 10) - 1);
  instr |= low10(addr);
  instr_at_put(pc + i * kInstructionSize, instr);
  i--; // i = 0
#endif
          
  addr >>= 10;  
  CHECK(i == 0);  
  instr = instr_at(pc + i * kInstructionSize);  // pc => sethi
  CHECK(inv_op(instr) == branch_op && inv_op2(instr) == sethi_op2); 
  instr &= ~((1 << 22) - 1);
  instr |= addr;
  instr_at_put(pc + i * kInstructionSize, instr);
 
  DCHECK(target_address_at(pc) == target); //Just checking
  if (icache_flush_mode != SKIP_ICACHE_FLUSH) {
    FlushICache(isolate, pc, kInstructionsPerPachableSet * kInstructionSize);
  }
}


void Assembler::GetCode(CodeDesc* desc) {
  DCHECK(pc_ <= reloc_info_writer.pos());  // No overlap.
  // Set up code descriptor.
  desc->buffer = buffer_;
  desc->buffer_size = buffer_size_;
  desc->instr_size = pc_offset();
//  DCHECK(desc->instr_size > 0);  // Zero-size code objects upset the system.
  desc->reloc_size =
      static_cast<int>((buffer_ + buffer_size_) - reloc_info_writer.pos());
  desc->origin = this;
  desc->constant_pool_size = 0;
}


void Assembler::Align(int m) {
  DCHECK(m >= 4 && base::bits::IsPowerOfTwo32(m));
  while ((pc_offset() & (m - 1)) != 0) {
    nop();
  }
}

// Labels refer to positions in the (to be) generated code.
// There are bound, linked, and unused labels.
//
// Bound labels refer to known positions in the already
// generated code. pos() is the position the label refers to.
//
// Linked labels refer to unknown positions in the code
// to be generated; pos() is the position of the last
// instruction using the label.


// The link chain is terminated by a negative code position (must be aligned)
const int kEndOfChain = -4;


// Return the offset of the branch destionation of instruction inst
// at offset pos.
// Should have pcs, but since all is relative, it works out.
int Assembler::target_at(int pos) {
  int inst = instr_at(pos);
  int link;
  switch (inv_op(inst)) {
  case call_op:        
      link = inv_wdisp(inst, 30);  
      break;
  case branch_op:
    switch (inv_op2(inst)) {
      case fbp_op2:    
          link = inv_wdisp(inst, 19);  
          break;
      case bp_op2:    
          link = inv_wdisp( inst, 19);  
          break;
      case fb_op2:     
          link = inv_wdisp( inst, 22);  
          break;
      case br_op2:     
          link = inv_wdisp( inst, 22);  
          break;
      case bpr_op2: 
        if (is_cbcond(inst))
          link = inv_wdisp10(inst);
        else
          link = inv_wdisp16(inst);
        break;
      default: 
          UNREACHABLE();
    }
    break;
  default:
   DCHECK(false);
   return -1;
 }
  if (link == 0) return kEndOfChain;
  return pos + link;
}


// Patch instruction inst at offset pos to refer to target_pos
// and return the resulting instruction.
// We should have pcs, not offsets, but since all is relative, it will work out
// OK.
void Assembler::target_at_put(int pos, int target_pos) {
  int inst = instr_at(pos);
  int offset = target_pos - pos;
  int m; // mask for displacement field
  int v; // new value for displacement field
  const int word_aligned_ones = -4;
  switch (inv_op(inst)) {
  case call_op:    
      m = wdisp(word_aligned_ones, 30);  
      v = wdisp(offset, 30); 
      break;
  case branch_op:
    switch (inv_op2(inst)) {
      case fbp_op2:    
          m = wdisp(  word_aligned_ones, 19);  
          v = wdisp(  offset, 19); 
          break;
      case bp_op2:     
          m = wdisp(  word_aligned_ones, 19);  
          v = wdisp(  offset, 19); 
          break;
      case fb_op2:     
          m = wdisp(  word_aligned_ones, 22);  
          v = wdisp(  offset, 22); 
          break;
      case br_op2:     
          m = wdisp(  word_aligned_ones, 22);  
          v = wdisp(  offset, 22); 
          break;
      case bpr_op2:
        if (is_cbcond(inst)) {
          m = wdisp10(word_aligned_ones);
          v = wdisp10(offset);
        } else {
          m = wdisp16(word_aligned_ones);
          v = wdisp16(offset);
        }
        break;
      default: 
          UNREACHABLE();
    }
    break;
  default:
    DCHECK(false);
    break;
  }
  instr_at_put(pos, (inst & ~m)  |  v);
 }    


void Assembler::bind_to(Label* L, int pos) {
  DCHECK(0 <= pos && pos <= pc_offset());  // must have a valid binding position
 // int32_t trampoline_pos = kInvalidSlotPos;
  while (L->is_linked()) {
    int fixup_pos = L->pos();
   // int32_t offset = pos - fixup_pos;
  //  int maxReach = max_reach_from(fixup_pos);
    next(L);  // call next before overwriting link with target at fixup_pos
 /*   if (maxReach && is_intn(offset, maxReach) == false) {
      if (trampoline_pos == kInvalidSlotPos) {
        trampoline_pos = get_trampoline_entry();
        CHECK(trampoline_pos != kInvalidSlotPos);
        target_at_put(trampoline_pos, pos);
      }
      target_at_put(fixup_pos, trampoline_pos);
    } else {*/
      target_at_put(fixup_pos, pos);
  //  }
  }
  L->bind_to(pos);
/*
  if (!trampoline_emitted_ && is_branch) {
    UntrackBranch();
  }
*/
  // Keep track of the last bound label so we don't eliminate any instructions
  // before a bound label.
  if (pos > last_bound_pos_) last_bound_pos_ = pos;
}


void Assembler::bind(Label* L) {
  DCHECK(!L->is_bound());  // label can only be bound once
  bind_to(L, pc_offset());
}


void Assembler::next(Label* L) {
  DCHECK(L->is_linked());
  int link = target_at(L->pos());
  if (link == kEndOfChain) {
    L->Unuse();
  } else {
    DCHECK(link >= 0);
    L->link_to(link);
  }
}


bool Assembler::is_near(Label* L) {
  DCHECK(L->is_bound());
  return true;
}


int Assembler::link(Label* L) {
  int position;
  if (L->is_bound()) {
    position = L->pos();
  } else {
    if (L->is_linked()) {
      position = L->pos();  // L's link
    } else {
      // was: target_pos = kEndOfChain;
      // However, using self to mark the first reference
      // should avoid most instances of branch offset overflow.  See
      // target_at() for where this is converted back to kEndOfChain.
      position = pc_offset();
    }
    L->link_to(pc_offset());
  }

  return position;
}


void Assembler::GrowBuffer() {
  if (!own_buffer_) FATAL("external code buffer is too small");

  // Compute new buffer size.
  CodeDesc desc;  // The new buffer.
  if (buffer_size_ < 1 * MB) {
    desc.buffer_size = 2*buffer_size_;
  } else {
    desc.buffer_size = buffer_size_ + 1*MB;
  }
  CHECK_GT(desc.buffer_size, 0);  // No overflow.

  // Set up new buffer.
  desc.buffer = NewArray<byte>(desc.buffer_size);
  desc.origin = this;

  desc.instr_size = pc_offset();
  desc.reloc_size =
      static_cast<int>((buffer_ + buffer_size_) - reloc_info_writer.pos());

  // Copy the data.
  intptr_t pc_delta = desc.buffer - buffer_;
  intptr_t rc_delta = (desc.buffer + desc.buffer_size) -
      (buffer_ + buffer_size_);
  MemMove(desc.buffer, buffer_, desc.instr_size);
  MemMove(reloc_info_writer.pos() + rc_delta,
              reloc_info_writer.pos(), desc.reloc_size);

  // Switch buffers.
  DeleteArray(buffer_);
  buffer_ = desc.buffer;
  buffer_size_ = desc.buffer_size;
  pc_ += pc_delta;
  reloc_info_writer.Reposition(reloc_info_writer.pos() + rc_delta,
                               reloc_info_writer.last_pc() + pc_delta);

  // Nothing else to do here since we keep all internal references and
  // deferred relocation entries relative to the buffer (until
  // EmitRelocations).
}


void Assembler::RecordRelocInfo(RelocInfo::Mode rmode, intptr_t data) {
  // We do not try to reuse pool constants.
  RelocInfo rinfo(isolate(), pc_, rmode, data, NULL);
  if (rmode >= RelocInfo::COMMENT &&
      rmode <= RelocInfo::DEBUG_BREAK_SLOT_AT_CALL) {
    // Adjust code for new modes.
    DCHECK(RelocInfo::IsDebugBreakSlot(rmode)
           || RelocInfo::IsComment(rmode)
           || RelocInfo::IsPosition(rmode));
    // These modes do not need an entry in the constant pool.
  }
  if (!RelocInfo::IsNone(rinfo.rmode())) {
    // Don't record external references unless the heap will be serialized.
    if (rmode == RelocInfo::EXTERNAL_REFERENCE &&
        !serializer_enabled() && !emit_debug_code()) {
      return;
    }
    DCHECK(buffer_space() >= kMaxRelocSize);  // Too late to grow buffer here.
    if (rmode == RelocInfo::CODE_TARGET_WITH_ID) {
      RelocInfo reloc_info_with_ast_id(isolate(), pc_, rmode,
                                       RecordedAstId().ToInt(), NULL);
      ClearRecordedAstId();
      reloc_info_writer.Write(&reloc_info_with_ast_id);
    } else {
      reloc_info_writer.Write(&rinfo);
    }
  }
}


/*


void Assembler::CodeTargetAlign() {
  // No advantage to aligning branch/call targets to more than
  // single instruction, that I am aware of.
  Align(4);
}

// Debugging.
int Assembler::RelocateInternalReference(RelocInfo::Mode rmode, byte* pc,
                                         intptr_t pc_delta) {
    WARNING("UNIMPLEMENTED");
}
*/
}  // namespace internal
}  // namespace v8

#endif  // V8_TARGET_ARCH_SPARC
