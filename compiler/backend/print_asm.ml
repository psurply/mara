(*
** print_asm.ml for Mara
**
** Copyright (C) 2013 Pierre Surply
** <pierre.surply@gmail.com>
**
** This file is part of Mara.
**
**    Mara is free software: you can redistribute it and/or modify
**    it under the terms of the GNU General Public License as published by
**    the Free Software Foundation, either version 3 of the License, or
**    (at your option) any later version.
**
**    Mara is distributed in the hope that it will be useful,
**    but WITHOUT ANY WARRANTY; without even the implied warranty of
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**    GNU General Public License for more details.
**
**    You should have received a copy of the GNU General Public License
**    along with Mara.  If not, see <http://www.gnu.org/licenses/>.
**
** Started on  Thu Dec 27 20:00:14 2012 Pierre Surply
**  Last update Sat Aug 17 14:37:17 2013 Pierre Surply
*)

open Printf
open Asm

let print_operand cout = function
  | NoneOp		-> ()
  | Reg r		-> fprintf cout "r%d" r
  | RegReg (l, r)	-> fprintf cout "r%d, r%d" l r
  | RegInt (l, r)	-> fprintf cout "r%d, %d" l r
  | IntReg (l, r)	-> fprintf cout "%d, r%d" l r
  | Im i		-> fprintf cout "%d" i
  | String s		-> fprintf cout "%s" s
  | StringReg (s, r)	-> fprintf cout "%s, r%d" s r
  | RegString (r, s)	-> fprintf cout "r%d, %s" r s

let rec print_code cout = function
  | Instr (op, operand) ->
      fprintf cout "\t%-.5s\t" (instr2str op);
      print_operand cout operand;
      fprintf cout "\n"
  | Label s		-> fprintf cout "%s:\n" s
  | Include s		-> fprintf cout "#include <%s>\n" s
  | Define (v, s)	-> fprintf cout "#define %s %s\n" v s
  | Comment s		-> fprintf cout "/* %s */\n" s
  | Dir (d, s)		-> fprintf cout ".%s\t%s\n" d s
  | Code c		-> fprintf cout "\t%s\n" c
  | MemAlloc (mem, offset) -> List.iter (print_code cout)
    (Mem.alloc (mem.Mem.size - offset))
  | MemFree (mem, offset) -> List.iter (print_code cout)
    (Mem.free (mem.Mem.size - offset))
  | MemLoad (v, mem) -> List.iter (print_code cout)
    (Mem.load mem v)
  | MemStore (v, mem) -> List.iter (print_code cout)
    (Mem.store mem v)
  | MemGetAddr (v, mem) -> List.iter (print_code cout)
    (Mem.get_addr mem v)
  | Putfb mem           -> List.iter (print_code cout)
    [Instr (Ldi, RegString(28, Printf.sprintf
      "lo8(RAMEND - %d)" (mem.Mem.size + 2)));
     Instr (Ldi, RegString(29, Printf.sprintf
       "hi8(RAMEND - %d)" (mem.Mem.size + 2)))]

let print_asm cout = List.iter (print_code cout)
