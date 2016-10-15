(*
** asm.ml for Mara
**
** Made by Pierre Surply
** <pierre.surply@gmail.com>
**
** Started on  Fri May 17 16:23:21 2013 Pierre Surply
**  Last update Sat Aug 17 14:35:52 2013 Pierre Surply
*)

open Conf

type operand =
| NoneOp
| Reg		of int
| RegReg	of int * int
| RegInt	of int * int
| IntReg	of int * int
| Im		of int
| String	of string
| StringReg	of string * int
| RegString	of int * string

type asminstr =
| Adc | Add | Adiw | And | Andi | Asr
| Bclr | Bld | Brbc | Brbs | Brcc | Brcs | Breq | Brge | Brhc | Brhs
| Brid | Brie | Brlo | Brlt | Brmi | Brne | Brpl | Brsh | Brtc | Brts
| Brvc | Brvs | Bset
| Call | Cbi | Cbr | Clc | Clh | Cli | Cln | Clr | Cls | Clt | Clv
| Clz | Com | Cp | Cpc | Cpi | Cpse
| Dec
| Eicall | Eijmp | Elpm | Eor
| Fmul | Fmuls | Fmulsu
| Icall | Ijmp | In | Inc
| Jmp
| Ld | Ldd | Ldi | Lds | Lpm | Lsl | Lsr
| Mov | Movw | Mul | Muls | Mulsu
| Neg | Nop
| Or | Ori | Out
| Pop | Push
| Rcall | Ret | Reti | Rjmp | Rol | Ror
| Sbc | Sbci | Sbi | Sbic | Sbis | Sbiw | Sbr | Sbrc | Sbrs | Sec | Seh | Sei
| Sen | Ser | Ses | Set | Sev | Sez | Sleep | St | Std | Sts | Sub | Subi | Swap
| Tst
| Wdr

type 'a t =
| Instr         of asminstr * operand
| Label         of string
| Include	of string
| Define	of string * string
| Comment	of string
| Dir		of string * string
| Code          of string
| MemAlloc      of 'a * int
| MemFree       of 'a * int
| MemStore      of string * 'a
| MemLoad       of string * 'a
| MemGetAddr    of string * 'a
| Putfb         of 'a

let instr2str = function
| Adc           -> "adc"
| Add           -> "add"
| Adiw          -> "adiw"
| And           -> "and"
| Andi          -> "andi"
| Asr           -> "asr"
| Bclr          -> "bclr"
| Bld           -> "bld"
| Brbc          -> "brbc"
| Brbs          -> "brbs"
| Brcc          -> "brcc"
| Brcs          -> "brcs"
| Breq          -> "breq"
| Brge          -> "brge"
| Brhc          -> "brhc"
| Brhs          -> "brhs"
| Brid          -> "brid"
| Brie          -> "brie"
| Brlo          -> "brlo"
| Brlt          -> "brlt"
| Brmi          -> "brmi"
| Brne          -> "brne"
| Brpl          -> "brpl"
| Brsh          -> "brsh"
| Brtc          -> "brtc"
| Brts          -> "brts"
| Brvc          -> "brvc"
| Brvs          -> "brvs"
| Bset          -> "bset"
| Call          ->
  begin
    match conf.mmcu with
    | "atmega8" | "atmega8515"
    | "atmega83" | "atmega85"   -> "rcall"
    | _                         -> "call"
  end
| Cbi           -> "cbi"
| Cbr           -> "cbr"
| Clc           -> "clc"
| Clh           -> "clh"
| Cli           -> "cli"
| Cln           -> "cln"
| Clr           -> "clr"
| Cls           -> "cls"
| Clt           -> "clt"
| Clv           -> "clv"
| Clz           -> "clz"
| Com           -> "com"
| Cp            -> "cp"
| Cpc           -> "cpc"
| Cpi           -> "cpi"
| Cpse          -> "cpse"
| Dec           -> "dec"
| Eicall        -> "eicall"
| Eijmp         -> "eijmp"
| Elpm          -> "elpm"
| Eor           -> "eor"
| Fmul          -> "fmul"
| Fmuls         -> "fmuls"
| Fmulsu        -> "fmulsu"
| Icall         -> "icall"
| Ijmp          -> "ijmp"
| In            -> "in"
| Inc           -> "inc"
| Jmp           -> "jmp"
| Ld            -> "ld"
| Ldd           -> "ldd"
| Ldi           -> "ldi"
| Lds           -> "lds"
| Lpm           -> "lpm"
| Lsl           -> "lsl"
| Lsr           -> "lsr"
| Mov           -> "mov"
| Movw          -> "movw"
| Mul           -> "mul"
| Muls          -> "muls"
| Mulsu         -> "mulsu"
| Neg           -> "neg"
| Nop           -> "nop"
| Or            -> "or"
| Ori           -> "ori"
| Out           -> "out"
| Pop           -> "pop"
| Push          -> "push"
| Rcall         -> "rcall"
| Ret           -> "ret"
| Reti          -> "reti"
| Rjmp          -> "rjmp"
| Rol           -> "rol"
| Ror           -> "ror"
| Sbc           -> "sbc"
| Sbci          -> "sbci"
| Sbi           -> "sbi"
| Sbic          -> "sbic"
| Sbis          -> "sbis"
| Sbiw          -> "sbiw"
| Sbr           -> "sbr"
| Sbrc          -> "sbrc"
| Sbrs          -> "sbrs"
| Sec           -> "sec"
| Seh           -> "seh"
| Sei           -> "sei"
| Sen           -> "sen"
| Ser           -> "ser"
| Ses           -> "ses"
| Set           -> "set"
| Sev           -> "sev"
| Sez           -> "sez"
| Sleep         -> "sleep"
| St            -> "st"
| Std           -> "std"
| Sts           -> "sts"
| Sub           -> "sub"
| Subi          -> "subi"
| Swap          -> "swap"
| Tst           -> "tst"
| Wdr           -> "wdr"
