(*
** mem.ml for Mara
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
** Started on  Fri Nov 23 21:21:30 2012 Pierre Surply
**  Last update Wed Aug 21 15:18:45 2013 Pierre Surply
*)

open Asm

type mem =
    {
      mutable size	: int;
      loc		: (string, int) Hashtbl.t;
      ty		:
	((string, Typing.type_name) Hashtbl.t) option;
    }

type cl_mem = (string, mem) Hashtbl.t

let create ty =
  {
    size = 0;
    loc = Hashtbl.create 13;
    ty = ty;
  }

let copy mem =
  {
    size = mem.size;
    loc = Hashtbl.copy mem.loc;
    ty = mem.ty;
  }

let get_loc mem v =
  try
    mem.size - (Hashtbl.find mem.loc v)
  with
    Not_found ->
      failwith "Mem get_loc"

let add ?size:(size=2) mem v =
  try
    let _ = Hashtbl.find mem.loc v in ()
  with
    Not_found ->
      begin
        mem.size <- mem.size + size;
        Hashtbl.add mem.loc v (mem.size - 1)
      end

let get_addr mem v =
  let loc = get_loc mem v in
  [Instr (Movw, RegReg(24, 28));
    Instr (Subi, RegString(24, Printf.sprintf "lo8(-(%d))" loc));
    Instr (Sbci, RegString(25, Printf.sprintf "hi8(-(%d))" loc));
  ]

let store mem v =
  let loc = get_loc mem v in
  let loc1 = string_of_int loc in
  let loc2 = string_of_int (loc+1) in
  if loc < 63 then
    [Instr (Std, StringReg("Y+" ^ loc1, 24));
     Instr (Std, StringReg("Y+" ^ loc2, 25));
    ]
  else
   [Instr (Movw, RegReg(30, 28));
    Instr (Subi, RegString(30, Printf.sprintf "lo8(-(%d))" loc));
    Instr (Sbci, RegString(31, Printf.sprintf "hi8(-(%d))" loc));
    Instr (St, StringReg("Z", 24));
    Instr (Std, StringReg("Z+1", 25));
   ]

let load mem v =
  let loc = get_loc mem v in
  let loc1 = string_of_int loc in
  let loc2 = string_of_int (loc+1) in
  if loc < 63 then
    [Instr (Ldd, RegString(24, "Y+" ^ loc1));
     Instr (Ldd, RegString(25, "Y+" ^ loc2));
    ]
  else
    [Instr (Movw, RegReg(30, 28));
     Instr (Subi, RegString(30, Printf.sprintf "lo8(-(%d))" loc));
     Instr (Sbci, RegString(31, Printf.sprintf "hi8(-(%d))" loc));
     Instr (Ld, RegString(24, "Z"));
     Instr (Ldd, RegString(25, "Z+1"))
    ]

let alloc size =
  [Comment ("Frame size : " ^ (string_of_int size))] @
    begin
      if size >= 8 then
        let rec sbiw size =
          if size > 63 then
            Instr (Sbiw, RegInt(28, 63))::sbiw (size - 63)
          else
            [Instr (Sbiw, RegInt(28, size))]
        in
        List.concat [[Instr (In, RegString (28, "__SP_L__"));
	              Instr (In, RegString (29, "__SP_H__"))];
                     sbiw size;
	             [Instr (In, RegString (0, "__SREG__"));
	              Instr (Cli, NoneOp);
	              Instr (Out, StringReg("__SP_H__", 29));
	              Instr (Out, StringReg("__SREG__", 0));
	              Instr (Out, StringReg("__SP_L__", 28))]
	            ]
      else
	let rec lrcall = function
	  | 0 | -1 -> []
	  | n ->
	      (Instr (Rcall, String "."))::lrcall (n-2)
	in
	  lrcall size@
	    [Instr (In, RegString (28, "__SP_L__"));
	     Instr (In, RegString (29, "__SP_H__"));
	    ]
    end

let free size =
  begin
    if size >= 8 then
      let rec adiw size =
        if size > 63 then
          Instr (Adiw, RegInt(28, 63))::adiw (size - 63)
        else
          [Instr (Adiw, RegInt(28, size))]
      in
      List.concat [[Instr (In, RegString (28, "__SP_L__"));
                    Instr (In, RegString (29, "__SP_H__"))];
                   adiw size;
                   [Instr (In, RegString (0, "__SREG__"));
                    Instr (Cli, NoneOp);
                    Instr (Out, StringReg("__SP_H__", 29));
                    Instr (Out, StringReg("__SREG__", 0));
                    Instr (Out, StringReg("__SP_L__", 28))]
                  ]
    else
      let rec lrcall = function
	| 0 -> []
	| n ->
	  (Instr (Pop, Reg 0))::lrcall (n-1)
      in
      lrcall size
  end
