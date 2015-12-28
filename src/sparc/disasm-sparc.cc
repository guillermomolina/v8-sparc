// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// A Disassembler object is used to disassemble a block of code instruction by
// instruction. The default implementation of the NameConverter object can be
// overriden to modify register names or to do symbol lookup on addresses.
//
// The example below will disassemble a block of code and print it to stdout.
//
//   NameConverter converter;
//   Disassembler d(converter);
//   for (byte* pc = begin; pc < end;) {
//     v8::internal::EmbeddedVector<char, 256> buffer;
//     byte* prev_pc = pc;
//     pc += d.InstructionDecode(buffer, pc);
//     printf("%p    %08x      %s\n",
//            prev_pc, *reinterpret_cast<int32_t*>(prev_pc), buffer);
//   }
//
// The Disassembler class also has a convenience method to disassemble a block
// of code into a FILE*, meaning that the above functionality could also be
// achieved by just calling Disassembler::Disassemble(stdout, begin, end);


#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#if V8_TARGET_ARCH_SPARC

#include "src/base/platform/platform.h"
#include "src/disasm.h"
#include "src/macro-assembler.h"
#include "src/sparc/constants-sparc.h"


namespace v8 {
namespace internal {


//------------------------------------------------------------------------------

// Decoder decodes and disassembles instructions into an output buffer.
// It uses the converter to convert register names and call destinations into
// more informative description.
class Decoder {
 public:
   Decoder(const disasm::NameConverter& converter,
          v8::internal::Vector<char> out_buffer)
    : converter_(converter),
      out_buffer_(out_buffer),
      out_buffer_pos_(0) {
    out_buffer_[out_buffer_pos_] = '\0';
  }

  ~Decoder() {}

  // Writes one disassembled instruction into 'buffer' (0-terminated).
  // Returns the length of the disassembled machine instruction in bytes.
  int InstructionDecode(byte* instruction);
  void DecodeCall(Instr* instr);
  void DecodeBranch(Instr* instr);
  void DecodeTrap(Instr* instr);
  void DecodeArithmetic(Instr* instr);
  void DecodeLoadStore(Instr* instr);

 private:
  // Bottleneck functions to print into the out_buffer.
  void PrintChar(const char ch);
  void Print(const char* str);

  // Printing of common values.
  void PrintRegister(Register reg);
  void PrintFPURegister(int freg);
  void PrintFPUStatusRegister(int freg);
  void PrintHi22(Instr* instr);
  void PrintSImm13(Instr* instr);
  void PrintShCnt32(Instr* instr);
  void PrintShCnt64(Instr* instr);
  void PrintTrapNumber(Instr* instr);
  void PrintDisp(Instr* instr, int nbits);
  void PrintDisp16(Instr* instr);

  // Handle formatting of instructions and their options.
  int FormatRegister(Instr* instr, const char* option);
  int FormatFPRegister(Instr* instr, const char* option);
  int FormatOption(Instr* instr, const char* option);
  void Format(Instr* instr, const char* format);
 /* void Unknown(Instruction* instr);
  void UnknownFormat(Instruction* instr, const char* opcname);*/

  const disasm::NameConverter& converter_;
  v8::internal::Vector<char> out_buffer_;
  int out_buffer_pos_;

  DISALLOW_COPY_AND_ASSIGN(Decoder);
};

// Support for assertions in the Decoder formatting functions.
#define STRING_STARTS_WITH(string, compare_string) \
  (strncmp(string, compare_string, strlen(compare_string)) == 0)


// Append the ch to the output buffer.
void Decoder::PrintChar(const char ch) {
  out_buffer_[out_buffer_pos_++] = ch;
}

// Append the str to the output buffer.
void Decoder::Print(const char* str) {
  char cur = *str++;
  while (cur != '\0' && (out_buffer_pos_ < (out_buffer_.length() - 1))) {
    PrintChar(cur);
    cur = *str++;
  }
  out_buffer_[out_buffer_pos_] = 0;
}

// Print 22-bit unsigned immediate value.
void Decoder::PrintHi22(Instr* instr) {
  int32_t imm = Assembler::inv_hi22(*instr);
  out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, "0x%x", imm);
}

// Print 13-bit signed immediate value.
void Decoder::PrintSImm13(Instr* instr) {
  int32_t imm = Assembler::inv_simm13(*instr);
  if(imm < 0)
    out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, "%d", imm);
  else
    out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, "0x%x", imm);
}

// Print 5-bit signed immediate value.
void Decoder::PrintShCnt32(Instr* instr) {
  int32_t imm = Assembler::inv_u_field(*instr, 4, 0);
  out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, "%d", imm);
}

// Print 5-bit signed immediate value.
void Decoder::PrintShCnt64(Instr* instr) {
  int32_t imm = Assembler::inv_u_field(*instr, 5, 0);
  out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, "%d", imm);
}

// Print 5-bit signed immediate value.
void Decoder::PrintTrapNumber(Instr* instr) {
  int32_t trap = Assembler::inv_u_field(*instr, 6, 0);
  out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, "0x%x", trap);
}

// Print n-bit signed displacement value.
void Decoder::PrintDisp(Instr* instr, int nbits) {
  int32_t disp = Assembler::inv_wdisp(*instr, nbits);
  out_buffer_pos_ +=
      SNPrintF(out_buffer_ + out_buffer_pos_, "%s",
               converter_.NameOfAddress(reinterpret_cast<byte*>(instr) + disp));
}

// Print 16-bit signed displacement value.
void Decoder::PrintDisp16(Instr* instr) {
  int32_t disp = Assembler::inv_wdisp16(*instr);
  out_buffer_pos_ +=
      SNPrintF(out_buffer_ + out_buffer_pos_, "%s",
               converter_.NameOfAddress(reinterpret_cast<byte*>(instr) + disp));
}

// Print the register name according to the active name converter.
void Decoder::PrintRegister(Register reg) {
  Print("%");
  Print(reg.ToString());
}

// Handle all register based formatting in this function to reduce the
// complexity of FormatOption.
int Decoder::FormatRegister(Instr* instr, const char* format) {
  DCHECK(format[0] == 'r');

  if (format[1] == 'd') {  // rd
    Register reg = Assembler::inv_rd(*instr);
    PrintRegister(reg);
    return 2;
  } 
  if (format[1] == 's') { // rs
    if (format[2] == '1') {  //rs1
      Register reg = Assembler::inv_rs1(*instr);
      PrintRegister(reg);
      return 3;
    }
    if (format[2] == '2') {  //rs2
      Register reg = Assembler::inv_rs2(*instr);
      PrintRegister(reg);
      return 3;
    }
  }

  UNREACHABLE();
  return -1;
}


// Handle all FP register based formatting in this function to reduce the
// complexity of FormatOption.
int Decoder::FormatFPRegister(Instr* instr, const char* format) {
/*  DCHECK(format[0] == 'D');

  int retval = 2;
  int reg = -1;
  if (format[1] == 't') {
    reg = instr->RTValue();
  } else if (format[1] == 'a') {
    reg = instr->RAValue();
  } else if (format[1] == 'b') {
    reg = instr->RBValue();
  } else if (format[1] == 'c') {
    reg = instr->RCValue();
  } else {*/
    UNREACHABLE();
    return -1;
 /* }

  PrintDRegister(reg);

  return retval;*/
}

// FormatOption takes a formatting string and interprets it based on
// the current instructions. The format string points to the first
// character of the option string (the option escape has already been
// consumed by the caller.)  FormatOption returns the number of
// characters that were consumed from the formatting string.
int Decoder::FormatOption(Instr* instr, const char* format) {
  switch (format[0]) {
    case 'r': {
      return FormatRegister(instr, format);
    }
    case 'D': {
      return FormatFPRegister(instr, format);
    }
    case 't': {
      DCHECK(STRING_STARTS_WITH(format, "trap"));
      PrintTrapNumber(instr);
      return 4;
    }
    case 's': {  
      if (format[4] == '1') { // 'simm13.
        if (format[5] == '3') {
          DCHECK(STRING_STARTS_WITH(format, "simm13"));
          PrintSImm13(instr);
          return 6;
        }
      }
      if (format[5] == '3') { //shcnt32
        if (format[6] == '2') {
          DCHECK(STRING_STARTS_WITH(format, "shcnt32"));
          PrintShCnt32(instr);
          return 7;
        }
      }
      if (format[5] == '6') { //shcnt64
        if (format[6] == '4') {
          DCHECK(STRING_STARTS_WITH(format, "shcnt64"));
          PrintShCnt64(instr);
          return 7;
        }
      }
      break;
    }
    case 'd': {  
      if (format[4] == '1') { // 'disp16.
        if (format[5] == '6') {
          DCHECK(STRING_STARTS_WITH(format, "disp16"));
          PrintDisp16(instr);
          return 6;
        }
        if (format[5] == '9') {
          DCHECK(STRING_STARTS_WITH(format, "disp19"));
          PrintDisp(instr, 19);
          return 6;
        }
      }
      if (format[4] == '2') { // 'disp22.
        if (format[5] == '2') {
          DCHECK(STRING_STARTS_WITH(format, "disp22"));
          PrintDisp(instr, 22);
          return 6;
        }
      }
      break;
    }
    case 'h': {   // 'hi22.
      DCHECK(STRING_STARTS_WITH(format, "hi22"));
      PrintHi22(instr);
      return 4;
    }
    default: {
      UNREACHABLE();
      break;
    }
  }

  UNREACHABLE();
  return -1;
}

// Format takes a formatting string for a whole instruction and prints it into
// the output buffer. All escaped options are handed to FormatOption to be
// parsed further.
void Decoder::Format(Instr* instr, const char* format) {
  char cur = *format++;
  while ((cur != 0) && (out_buffer_pos_ < (out_buffer_.length() - 1))) {
    if (cur == '\'') {  // Single quote is used as the formatting escape.
      format += FormatOption(instr, format);
    } else {
      out_buffer_[out_buffer_pos_++] = cur;
    }
    cur = *format++;
  }
  out_buffer_[out_buffer_pos_] = '\0';
}

void Decoder::DecodeCall(Instr* instr) {
  UNIMPLEMENTED();
}

void Decoder::DecodeBranch(Instr* instr) {
  switch (Assembler::inv_op2(*instr)) {
  case bpr_op2: 
    switch(Assembler::inv_u_field(*instr, 27, 25))   {
    case rc_z:
      Print("brz");
      break;
    case rc_lez:
      Print("brlez");
      break;
    case rc_lz:
      Print("brlz");
      break;
    case rc_nz:
      Print("brnz");
      break;
    case rc_gz:
      Print("brgz");
      break;
    case rc_gez:
      Print("brgez");
      break;
    default:
      UNREACHABLE();
    }
    if(Assembler::inv_u_field(*instr, 29, 29) == 1)
      Print(",a");
    if(Assembler::inv_u_field(*instr, 19, 19) == 1)
      Print(",pt");
    else
      Print(",pn");
    Format(instr, "    'rs1, 'disp16");
    break;
  case br_op2: 
    switch(Assembler::inv_cond(*instr))   {
    case never:
      Print("bn");
      break;
    case zero:
      Print("bz");
      break;
    case lessEqual:
      Print("ble");
      break;
    case less:
      Print("bl");
      break;
    case lessEqualUnsigned:
      Print("bleu");
      break;
    case negative:
      Print("bneg");
      break;
    case overflowSet:
      Print("bvs");
      break;
    case always:
      Print("ba");
      break;
    case notEqual:
      Print("bne");
      break;
    case greater:
      Print("bg");
      break;
    case greaterEqual:
      Print("bge");
      break;
    case greaterUnsigned:
      Print("bgu");
      break;
    case carryClear:
      Print("bcc");
      break;
    case positive:
      Print("bpos");
      break;
    case overflowClear:
      Print("bvc");
      break;
    default:
      UNREACHABLE();
    }
    if(Assembler::inv_u_field(*instr, 29, 29) == 1)
      Print(",a");
    Format(instr, "    'rs1, 'disp22");
    break;
  case bp_op2: 
    switch(Assembler::inv_cond(*instr))   {
    case never:
      Print("bpn");
      break;
    case zero:
      Print("bpz");
      break;
    case lessEqual:
      Print("bple");
      break;
    case less:
      Print("bpl");
      break;
    case lessEqualUnsigned:
      Print("bpleu");
      break;
    case negative:
      Print("bpneg");
      break;
    case overflowSet:
      Print("bpvs");
      break;
    case always:
      Print("bpa");
      break;
    case notEqual:
      Print("bpne");
      break;
    case greater:
      Print("bpg");
      break;
    case greaterEqual:
      Print("bge");
      break;
    case greaterUnsigned:
      Print("bpgu");
      break;
    case carryClear:
      Print("bpcc");
      break;
    case positive:
      Print("bppos");
      break;
    case overflowClear:
      Print("bpvc");
      break;
    default:
      UNREACHABLE();
    }
    if(Assembler::inv_u_field(*instr, 29, 29) == 1)
      Print(",a");
    if(Assembler::inv_u_field(*instr, 19, 19) == 1)
      Print(",pt");
    else
      Print(",pn");
    if(Assembler::inv_u_field(*instr, 20, 20) == 1)
      UNREACHABLE();
    if(Assembler::inv_u_field(*instr, 21, 21) == 1)
      Format(instr, "    %xcc, 'disp19");
    else
      Format(instr, "    %icc, 'disp19");
    break;
  case sethi_op2:
    if(*instr == 0x1000000)
      Print("nop");
    else
      Format(instr, "sethi    %hi('hi22), 'rd");
    break;
  case fbp_op2: 
    Print("fbp");
    UNIMPLEMENTED();
    break;
  case fb_op2: 
    Print("fb");
    UNIMPLEMENTED();
    break;
  default:
    UNREACHABLE();
  }          
}


void Decoder::DecodeTrap(Instr* instr) {
  switch(Assembler::inv_cond(*instr))   {
  case never:
    Print("tn");
    break;
  case zero:
    Print("tz");
    break;
  case lessEqual:
    Print("tle");
    break;
  case less:
    Print("tl");
    break;
  case lessEqualUnsigned:
    Print("tleu");
    break;
  case negative:
    Print("tneg");
    break;
  case overflowSet:
    Print("tvs");
    break;
  case always:
    Print("ta");
    break;
  case notEqual:
    Print("tne");
    break;
  case greater:
    Print("tg");
    break;
  case greaterEqual:
    Print("bge");
    break;
  case greaterUnsigned:
    Print("tgu");
    break;
  case carryClear:
    Print("tcc");
    break;
  case positive:
    Print("tpos");
    break;
  case overflowClear:
    Print("tvc");
    break;
  default:
    UNREACHABLE();
  }
  if(Assembler::inv_u_field(*instr, 11,11) == 1)
    UNREACHABLE();
  if(Assembler::inv_u_field(*instr, 12, 12) == 1)
    Format(instr, "    xcc, ");
  else
    Format(instr, "    icc, ");
  if(Assembler::inv_immed(*instr) == 1)
    Format(instr, "'trap");
  else
    Format(instr, "'rs1");       
}

void Decoder::DecodeArithmetic(Instr* instr) {
  switch (Assembler::inv_op3(*instr)) {
  case add_op3:  // 0
    Print("add");
    break;
  case and_op3: // 1
    Print("and");
    break;
  case or_op3: // 2
    Print("or");
    break;
  case xor_op3: // 3
    Print("xor");
    break;
  case sub_op3: // 4
    Print("sub");
    break;
  case andn_op3: // 5
    Print("andn");
    break;
  case orn_op3:  // 6
    Print("orn");
    break;
  case xnor_op3: // 7
    Print("xnor");
    break;
  case addc_op3: // 8
    Print("addc");
    break;
  case mulx_op3: // 9
    Print("mulx");
    break;
  case umul_op3: // a
    Print("umul");
    break;
  case smul_op3: // b
    Print("smul");
    break;
  case subc_op3:  // c
    Print("subc");
    break;
  case udivx_op3: // d
    Print("udivx");
    break;
  case udiv_op3: // e
    Print("udiv");
    break;
  case sdiv_op3: // f
    Print("sdiv");
    break;
        
  case addcc_op3:  // 10
    Print("addcc");
    break;
  case andcc_op3: // 11
    Print("andcc");
    break;
  case orcc_op3: // 12
    Print("orcc");
    break;
  case xorcc_op3: // 13
    Print("xorcc");
    break;
  case subcc_op3: // 14
    Print("subcc");
    break;
  case andncc_op3: // 15
    Print("andncc");
    break;
  case orncc_op3:  // 16
    Print("orncc");
    break;
  case xnorcc_op3: // 17
    Print("xnorcc");
    break;
  case addccc_op3: // 18
    Print("addccc");
    break;
  case aes4_op3: // 19
    Print("aes4");
    UNIMPLEMENTED();
    break;
  case umulcc_op3: // 1a
    Print("umulcc");
    UNIMPLEMENTED();
    break;
  case smulcc_op3: // 1b
    Print("smulcc");
    UNIMPLEMENTED();
    break;
  case subccc_op3:  // 1c
    Print("subccc");
    UNIMPLEMENTED();
    break;
                               // 1d
         
  case udivcc_op3: // 1e
    Print("udivcc");
    UNIMPLEMENTED();
    break;
  case sdivcc_op3: // 1f
    Print("sdivcc");
    UNIMPLEMENTED();
    break;
      
  case taddcc_op3:  // 20
    Print("taddcc");
    UNIMPLEMENTED();
    break;
  case tsubcc_op3: // 21
    Print("tsubcc");
    UNIMPLEMENTED();
    break;
  case taddcctv_op3: // 22
    Print("taddcctv");
    UNIMPLEMENTED();
    break;
  case tsubcctv_op3: // 23
    Print("tsubcctv");
    UNIMPLEMENTED();
    break;
  case mulscc_op3: // 24
    Print("mulscc");
    UNIMPLEMENTED();
    break;
  case sllx_op3: // 25
    if(Assembler::inv_u_field(*instr, 12, 12) == 0)  {
      Print("sll");
      if(Assembler::inv_immed(*instr)) {
        Format(instr, "    'rs1, 'shcnt32, 'rd");
        return;
      }
    }
    else {
      Print("sllx");
      if(Assembler::inv_immed(*instr)) {
        Format(instr, "    'rs1, 'shcnt64, 'rd");
        return;
      }
    }
    break;
  case srlx_op3:  // 26
    if(Assembler::inv_u_field(*instr, 12, 12) == 0)  {
      Print("srl");
      if(Assembler::inv_immed(*instr)) {
        Format(instr, "    'rs1, 'shcnt32, 'rd");
        return;
      }
    }
    else {
      Print("srlx");
      if(Assembler::inv_immed(*instr)) {
        Format(instr, "    'rs1, 'shcnt64, 'rd");
        return;
      }
    }
    break;
  case srax_op3: // 27
    if(Assembler::inv_u_field(*instr, 12, 12) == 0)  {
      Print("sra");
      if(Assembler::inv_immed(*instr)) {
        Format(instr, "    'rs1, 'shcnt32, 'rd");
        return;
      }
    }
    else {
      Print("srax");
      if(Assembler::inv_immed(*instr)) {
        Format(instr, "    'rs1, 'shcnt64, 'rd");
        return;
      }
    }
    break;
  case membar_op3: // 28
    Print("membar");
    UNIMPLEMENTED();
    return;
                                 // 29
                                 // 2a
  case flushw_op3: // 2b
    Print("flushw");
    UNIMPLEMENTED();
    break;
  case movcc_op3:  // 2c
    Print("movcc");
    UNIMPLEMENTED();
    break;
  case sdivx_op3: // 2d
    Print("sdivx");
    UNIMPLEMENTED();
    break;
  case popc_op3: // 2e
    Print("popc");
    UNIMPLEMENTED();
    break;
  case movr_op3: // 2f
    Print("movr");
    UNIMPLEMENTED();
    break;

  case jmpl_op3: // 38
    if(*instr == 0x81c7e008) {
      Print("ret");
      return;
    }
    if(Assembler::inv_rd(*instr).is(g0)) {
      if(Assembler::inv_immed(*instr))
        Format(instr, "jmp    'rs1, 'simm13");
      else
        Format(instr, "jmp    'rs1, 'rs2");
      return;
    }
    if(Assembler::inv_rd(*instr).is(o7)) {
      if(Assembler::inv_immed(*instr))
        Format(instr, "call    'rs1, 'simm13");
      else
        Format(instr, "call    'rs1, 'rs2");
      return;
    }
    Print("jmpl");
    break;
  case rett_op3: // 39
    Print("rett");
    break;
    
  case trap_op3: // 3A
    DecodeTrap(instr);
    return;

  case save_op3: // 3c
    Print("save");
    break;
  case restore_op3: // 3d
    Print("restore");
    if(*instr==0x81e80000)
      return;
    break;

  default:
      UNREACHABLE();
  }          
  if(Assembler::inv_immed(*instr))
    Format(instr, "    'rs1, 'simm13, 'rd");
  else
    Format(instr, "    'rs1, 'rs2, 'rd");
}

void Decoder::DecodeLoadStore(Instr* instr) {
  bool is_load_integer = true; 
  switch (Assembler::inv_op3(*instr)) {
  case lduw_op3:  // 0
    Print("lduw");
    break;
  case ldub_op3: // 1
    Print("ldub");
    break;
  case lduh_op3: // 2
    Print("lduh");
    break;
  case ldd_op3: // 3
    Print("ldd");
    break;
  case ldsw_op3: // 8
    Print("ldsw");
    break;
  case ldsb_op3: // 9
    Print("ldsb");
    break;
  case ldsh_op3: // a
    Print("ldsh");
    break;
  case ldx_op3: // b
    Print("ldx");
    break;

  default:
      is_load_integer = false;
  }          
  if(is_load_integer) {
    if(Assembler::inv_immed(*instr))
      Format(instr, "    [ 'rs1 + 'simm13 ], 'rd");
    else
      Format(instr, "    [ 'rs1 + 'rs2 ], 'rd"); 
    return;
  }
  bool is_store_integer = true; 
  switch (Assembler::inv_op3(*instr)) {
  case stw_op3: // 4
    Print("stw");
    break;
  case stb_op3: // 5
    Print("stb");
    break;
  case sth_op3:  // 6
    Print("sth");
    break;
  case std_op3: // 7
    Print("std");
    break;
  case stx_op3: // e
    Print("stx");
    break;
  default:
      is_store_integer = false;
  }          
  if(is_store_integer) {
    if(Assembler::inv_immed(*instr))
      Format(instr, "    'rd, [ 'rs1 + 'simm13 ]");
    else
      Format(instr, "    'rd, [ 'rs1 + 'rs2 ]"); 
    return;
  }
  switch (Assembler::inv_op3(*instr)) {
  case swap_op3: // f
    Print("swap");
    UNIMPLEMENTED();
    break;
  case stwa_op3: // 14
    Print("stwa");
    UNIMPLEMENTED();
    break;
  case stxa_op3: // 1e
    Print("stxa");
    UNIMPLEMENTED();
    break;
      
  case ldf_op3:  // 20
    Print("ldf");
    UNIMPLEMENTED();
    break;
  case ldfsr_op3: // 21
    Print("ldfsr");
    UNIMPLEMENTED();
    break;
  case ldqf_op3: // 22
    Print("ldqf");
    UNIMPLEMENTED();
    break;
  case lddf_op3: // 23
    Print("lddf");
    UNIMPLEMENTED();
    break;
  case stf_op3: // 24
    Print("stf");
    UNIMPLEMENTED();
    break;
  case stfsr_op3: // 25
    Print("stfsr");
    UNIMPLEMENTED();
    break;
  case stqf_op3:  // 26
    Print("stqf");
    UNIMPLEMENTED();
    break;
  case stdf_op3: // 27
    Print("stdf");
    UNIMPLEMENTED();
    break;

  case prefetch_op3: // 2d
    Print("prefetch");
    UNIMPLEMENTED();
    break;

  case casa_op3: // 3c
    Print("casa");
    UNIMPLEMENTED();
    break;
  case casxa_op3: // 3d
    Print("casxa");
    UNIMPLEMENTED();
    break;

  case mftoi_op3: // 3d
    Print("mftoi");
    UNIMPLEMENTED();
    break;

  default:
      UNREACHABLE();
  }
}


// Disassemble the instruction at *instr_ptr into the output buffer.
int Decoder::InstructionDecode(byte* instr_ptr) {
  Instr* instr = reinterpret_cast<Instr*>(instr_ptr);

  // Print raw instruction bytes.
  out_buffer_pos_ += SNPrintF(out_buffer_ + out_buffer_pos_, "%08x       ", *instr);

  switch (Assembler::inv_op(*instr)) {
  case call_op: // fmt 1
    DecodeCall(instr);
    break;
  case branch_op: // also sethi (fmt2)
    DecodeBranch(instr);
    break;
  case arith_op: // fmt 3, arith & misc
    DecodeArithmetic(instr);
    break;
  case ldst_op:  // fmt 3, load/store
    DecodeLoadStore(instr);
    break;
  default:
    UNREACHABLE();
  }
  
  return kInstructionSize;
}

#undef STRING_STARTS_WITH

}  // namespace internal
}  // namespace v8

    //------------------------------------------------------------------------------

namespace disasm {
    
const char* NameConverter::NameOfAddress(byte* addr) const {
  v8::internal::SNPrintF(tmp_buffer_, "%p", addr);
  return tmp_buffer_.start();
}


const char* NameConverter::NameOfConstant(byte* addr) const {
  UNIMPLEMENTED();
}


const char* NameConverter::NameOfCPURegister(int reg) const {
  UNIMPLEMENTED();
}


const char* NameConverter::NameOfXMMRegister(int reg) const {
  UNIMPLEMENTED();
}


const char* NameConverter::NameOfByteCPURegister(int reg) const {
  UNIMPLEMENTED();
}


const char* NameConverter::NameInCode(byte* addr) const {
  UNIMPLEMENTED();
}


//------------------------------------------------------------------------------

Disassembler::Disassembler(const NameConverter& converter)
    : converter_(converter) {}


int Disassembler::InstructionDecode(v8::internal::Vector<char> buffer,
                                    byte* instruction) {
  v8::internal::Decoder d(converter_, buffer);
  return d.InstructionDecode(instruction);
}

Disassembler::~Disassembler() {}



// The SPARC assembler does not currently use constant pools.
int Disassembler::ConstantPoolSizeAt(byte* instruction) {
  return -1;
}

void Disassembler::Disassemble(FILE* f, byte* begin, byte* end) {
  NameConverter converter;
  Disassembler d(converter);
  for (byte* pc = begin; pc < end;) {
    v8::internal::EmbeddedVector<char, 128> buffer;
    buffer[0] = '\0';
    byte* prev_pc = pc;
    pc += d.InstructionDecode(buffer, pc);
    v8::internal::PrintF(f, "%p    %08x      %s\n",
        prev_pc, *reinterpret_cast<int32_t*>(prev_pc), buffer.start());
  }
}


}  // namespace disasm

#endif  // V8_TARGET_ARCH_SPARC
