(*
** gen.ml for Mara
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
** Started on  Thu Dec 27 19:13:52 2012 Pierre Surply
**  Last update Thu Aug 22 15:17:41 2013 Pierre Surply
*)

open Ast
open Asm
open Print_asm
open Conf

type 'a constant =
  | CString of string
  | CVect of 'a list * Mem.mem

let mapi f l =
  let rec r_mapi i = function
    | [] -> []
    | h::t ->
      (f i h)::r_mapi (i + 1) t
  in
  r_mapi 0 l

let header cout =
  print_asm cout
    [ Define("__SFR_OFFSET", "0x20");
      Include "stdlib.h";
      Include "avr/io.h";
      Define("__SREG__", "0x3F");
      Define("__SP_H__", "0x3E");
      Define("__SP_L__", "0x3D")
    ]

let label = ref 0
let gbl_lbl id = "L" ^ (string_of_int id)

let static = Queue.create ()
let cl_mem = Hashtbl.create 13

let strlen s =
  let i = ref 0 in
  String.iter (fun  c -> if c != '\\' then incr i) s;
  !i + 1

let p_static () =
  if not (Queue.is_empty static) then
    let l = ref [] in
    let id = ref 0 in
    while not (Queue.is_empty static) do
      let label = ".LC" ^ (string_of_int !id) in
      begin
        match Queue.take static with
        | CString data ->
          l := [Label label;
	        Dir ("string", "\"" ^ data ^ "\"")]@(!l);
        | CVect (data, mem) ->
          let progdata = function
            | Int (i, _) ->
              Dir ("word", string_of_int i)
            | Var (v, _) ->
              begin
                try
                  let _ = Hashtbl.find mem.Mem.loc v in
                  Dir ("word", "0")
                with Not_found ->
                  Dir ("word", "gs(" ^ v ^ ")")
              end
            | _ ->
              Dir ("word", "0")
          in
          l := List.concat [[Label label;
                             Dir ("word", string_of_int (List.length data))];
                            List.map progdata data;
                            !l]
      end;
      incr id
    done;
    (Dir ("section", ".progmem.data,\"a\",@progbits"))::!l
  else []

let rec p_expr mem = function
  | Cast (_, e, _) -> p_expr mem e
  | ENone _ -> [Instr (Clr, Reg(24));
                Instr (Clr, Reg(25))]
  | Int	(i, _) ->
      [Instr (Ldi, RegInt(24, i land 0xFF));
       Instr (Ldi, RegInt(25, (i lsr 8) land 0xFF))]
  | Vect (el, _) ->
    let label = ".LC" ^ (string_of_int (Queue.length static)) in
    let size = (List.length el) * 2 + 2 in
    Mem.add ~size:size mem label;
    Queue.push (CVect (el, mem)) static;
    let set_elt i e =
      List.concat [[MemGetAddr (label, mem);
                    Instr (Subi, RegString(24, Printf.sprintf
                      "lo8(-(%d))" (i * 2 + 2)));
                    Instr (Sbci, RegString(25, Printf.sprintf
                      "hi8(-(%d))" (i * 2 + 2)));
                    Instr (Push, Reg(24));
                    Instr (Push, Reg(25));
                   ];
                   p_expr mem e;
                   [Instr (Pop, Reg(31));
                    Instr (Pop, Reg(30));
                    Instr (St, StringReg("Z", 24));
                    Instr (Std, StringReg("Z+1", 25))]
                  ]
    in
    let check_elt i = function
      | Int (_, _) -> []
      | Var (v, _) as e ->
        begin
          try
            let _ = Hashtbl.find mem.Mem.loc v in
            set_elt i e
          with Not_found -> []
        end
      | e -> set_elt i e
    in
    List.concat [[Instr (Ldi, RegString(22, "lo8(" ^ label ^ ")"));
                  Instr (Ldi, RegString(23, "hi8(" ^ label ^ ")"));
                  MemGetAddr (label, mem);
                  Instr (Ldi, RegString(20, Printf.sprintf "lo8(%d)" size));
                  Instr (Ldi, RegString(21, Printf.sprintf "hi8(%d)" size));
                  Instr (Call, String("memcpy_P"))];
                 List.concat (mapi check_elt el);
                 [MemGetAddr (label, mem)]
                ]
  | Str	(s, _) ->
    let label = ".LC" ^ (string_of_int (Queue.length static)) in
    let size = strlen s in
    Mem.add ~size:size mem label;
    Queue.push (CString s) static;
    [Instr (Ldi, RegString(22, "lo8(" ^ label ^ ")"));
     Instr (Ldi, RegString(23, "hi8(" ^ label ^ ")"));
     MemGetAddr (label, mem);
     Instr (Ldi, RegString(20, Printf.sprintf "lo8(%d)" size));
     Instr (Ldi, RegString(21, Printf.sprintf "hi8(%d)" size));
     Instr (Call, String("memcpy_P"));
     MemGetAddr (label, mem)]
  | Var (v, _) ->
    begin
      try
        let _ = Hashtbl.find mem.Mem.loc v in
        [MemLoad (v, mem)]
      with Not_found ->
        [Instr (Ldi, RegString(24, "lo8(gs(" ^ v ^ "))"));
         Instr (Ldi, RegString(25, "hi8(gs(" ^ v ^ "))"))]
    end
  | Addr (a, _) ->
    [Instr (Lds,
            match a with
            | AString s  -> RegString(24, s)
            | ADString (_, l)  -> RegString(24, l)
            | AInt i     -> RegInt(24, i)
            | ADInt (_, l)  -> RegInt(24, l));
     match a with
     | ADString (h, _) -> Instr (Lds, RegString(25, h))
     | ADInt (h, _) -> Instr (Lds, RegInt(25, h))
     | _ -> Instr (Clr, Reg(25))]
  | Const (c, _) ->
    let rec s = function
      | [] -> failwith "const gen"
      | h::[] -> "(1 << " ^ h ^ ")"
      | h::t -> "(1 << " ^ h ^ ") | " ^ (s t)
    in
    [Instr (Ldi, RegString(24, s c));
     Instr (Clr, Reg(25))]
  | Lambda (_, _, name, _) ->
    [Instr (Ldi, RegString(24, "lo8(gs(" ^ !name ^ "))"));
     Instr (Ldi, RegString(25, "hi8(gs(" ^ !name ^ "))"))]
  | Get (id, e, size, mc, _) ->
    begin
      match !mc with
      | None ->
        List.concat [p_expr mem id;
                     [Instr (Push, Reg(24));
                      Instr (Push, Reg(25))];
                     p_expr mem e;
                     begin
                       match !size with
                       | 1 -> []
                       | 2 -> [Instr (Adiw, RegInt(24, 1));
                               Instr (Lsl, Reg 24);
		               Instr (Rol, Reg 25)]
                       | _ -> failwith "gen get"
                     end;
                     [Instr (Pop, Reg(31));
                      Instr (Pop, Reg(30));
                      Instr (Add, RegReg(30, 24));
                      Instr (Adc, RegReg(31, 25))];
                     match !size with
                     | 1 ->
                       [Instr (Ld, RegString(24, "Z"));
                        Instr (Clr, Reg(25))]
                     | 2 ->
                       [Instr (Ld, RegString(24, "Z"));
                        Instr (Ldd, RegString(25, "Z+1"))]
                     | _ -> failwith "gen get"
                    ]
      | Some e ->
        p_expr mem e
    end
  | Field (e, f, ty, _) ->
    let ty = !ty in
    let loc =
        (Hashtbl.find
	   (Hashtbl.find cl_mem ty).Mem.loc f) - 1
    in
    p_expr mem e@
      [Instr (Adiw, RegInt (24,loc));
       Instr (Movw, RegReg(30, 24));
       Instr (Ld, RegString(24, "Z"));
       Instr (Ldd, RegString(25, "Z+1"))]
  | BinOp (op, le, re, _) ->
    let lbl_tmp = !label in
    begin
      match op with
      | "or" | "and" -> label := !label + 1
      | _ -> ()
    end;
    List.concat
      [p_expr mem le;
        begin
          match op with
          | "or" ->
            [Instr (Cp, RegReg(24, 1));
             Instr (Cpc, RegReg(25, 1));
             Instr (Breq, String ("1f"));
             Instr (Rjmp, String (gbl_lbl lbl_tmp));
             Label ("1")
            ]
          | "and" ->
            [Instr (Cp, RegReg(24, 1));
             Instr (Cpc, RegReg(25, 1));
             Instr (Brne, String ("1f"));
             Instr (Rjmp, String (gbl_lbl lbl_tmp));
             Label ("1")
            ]
          | _  -> []
        end;
        [Instr (Push, Reg 24);
         Instr (Push, Reg 25)];
	p_expr mem re;
        [Instr (Movw, RegReg(18, 24));
         Instr (Pop, Reg 25);
         Instr (Pop, Reg 24)];
	begin
	  match op with
	    | "+" -> [Instr (Add, RegReg(24, 18));
		      Instr (Adc, RegReg(25, 19))]
	    | "-" -> [Instr (Sub, RegReg(24, 18));
		      Instr (Sbc, RegReg(25, 19))]
	    | "*" -> [Instr (Movw, RegReg(20, 24));
		      Instr (Mul, RegReg(20, 18));
		      Instr (Movw, RegReg(24, 0));
		      Instr (Mul, RegReg(20, 19));
		      Instr (Add, RegReg(25, 0));
		      Instr (Mul, RegReg(21, 18));
		      Instr (Add, RegReg(25, 0));
		      Instr (Clr, Reg(1))]
	    | "/" -> [Instr (Movw, RegReg(22, 18));
		      Instr (Call, String "__divmodhi4");
		      Instr (Movw, RegReg(24, 22))]
	    | "%" -> [Instr (Movw, RegReg(22, 18));
		      Instr (Call, String "__divmodhi4")]
	    | "|" -> [Instr (Or, RegReg(24, 18));
		      Instr (Or, RegReg(25, 19))]
	    | "&" -> [Instr (And, RegReg(24, 18));
		      Instr (And, RegReg(25, 19))]
	    | "^" -> [Instr (Eor, RegReg(24, 18));
		      Instr (Eor, RegReg(25, 19))]
	    | "<<" -> [Instr (Mov, RegReg(0, 18));
		       Instr (Rjmp, String "2f");
		       Label "1";
		       Instr (Lsl, Reg 24);
		       Instr (Rol, Reg 25);
		       Label "2";
		       Instr (Dec, Reg 0);
		       Instr (Brpl, String "1b")]
	    | ">>" -> [Instr (Mov, RegReg(0, 18));
		       Instr (Rjmp, String "2f");
		       Label "1";
		       Instr (Lsr, Reg 25);
		       Instr (Ror, Reg 24);
		       Label "2";
		       Instr (Dec, Reg 0);
		       Instr (Brpl, String "1b")]
	    | "or" -> [Instr (Cp, RegReg(18, 1));
		       Instr (Cpc, RegReg(19, 1));
		       Instr (Brne, String(gbl_lbl lbl_tmp));
		       Instr (Clr, Reg 24);
		       Instr (Rjmp, String "2f");
                       Label (gbl_lbl lbl_tmp);
		       Instr (Ldi, RegInt (24, 1));
		       Label "2";
		       Instr (Clr, Reg 25);
                      ]
	    | "and" -> [Instr (Cp, RegReg(18, 1));
			Instr (Cpc, RegReg(19, 1));
			Instr (Breq, String(gbl_lbl lbl_tmp));
			Instr (Ldi, RegInt (24, 1));
			Instr (Rjmp, String "1f");
                        Label (gbl_lbl lbl_tmp);
			Instr (Clr, Reg 24);
			Label "1";
			Instr (Clr, Reg 25);
                       ]
	    | ">" -> [Instr (Cp, RegReg(24, 18));
                      Instr (Cpc, RegReg(25, 19));
                      Instr (Brlt, String "1f");
                      Instr (Breq, String "1f");
                      Instr (Ldi, RegInt (24, 1));
                      Instr (Rjmp, String "2f");
                      Label "1";
                      Instr (Clr, Reg (24));
                      Label "2";
                      Instr (Clr, Reg (25));
                      ]
	    | ">=" -> [Instr (Cp, RegReg(24, 18));
		       Instr (Cpc, RegReg(25, 19));
		       Instr (Brge, String "1f");
                       Instr (Clr, Reg (24));
		       Instr (Rjmp, String "2f");
		       Label "1";
		       Instr (Ldi, RegInt (24, 1));
		       Label "2";
                       Instr (Clr, Reg (25));
		      ]
	    | "<" -> [Instr (Cp, RegReg(24, 18));
		      Instr (Cpc, RegReg(25, 19));
		      Instr (Brlt, String "1f");
		      Instr (Clr, Reg (24));
		      Instr (Rjmp, String "2f");
		      Label "1";
		      Instr (Ldi, RegInt (24, 1));
		      Label "2";
		      Instr (Clr, Reg (25));
		     ]
	    | "<=" -> [Instr (Cp, RegReg(24, 18));
		       Instr (Cpc, RegReg(25, 19));
		       Instr (Brlt, String "1f");
		       Instr (Breq, String "1f");
		       Instr (Clr, Reg (24));
		       Instr (Rjmp, String "2f");
		       Label "1";
		       Instr (Ldi, RegInt (24, 1));
		       Label "2";
                       Instr (Clr, Reg (25));
		     ]
	    | "==" -> [Instr (Cp, RegReg(24, 18));
		      Instr (Cpc, RegReg(25, 19));
		      Instr (Breq, String "1f");
		      Instr (Clr, Reg (24));
		      Instr (Rjmp, String "2f");
		      Label "1";
		      Instr (Ldi, RegInt (24, 1));
		      Label "2";
		      Instr (Clr, Reg (25));
		     ]
	    | "!=" -> [Instr (Cp, RegReg(24, 18));
		      Instr (Cpc, RegReg(25, 19));
		      Instr (Brne, String "1f");
		      Instr (Clr, Reg (24));
		      Instr (Rjmp, String "2f");
		      Label "1";
		      Instr (Ldi, RegInt (24, 1));
		      Label "2";
		      Instr (Clr, Reg (25));
                      ]
	    | _ -> []
	end]
  | UniOp (op, e, _) ->
      p_expr mem e@
	begin
	  match op with
	    | "-" -> [Instr (Com, Reg(25));
		      Instr (Neg, Reg(24));
		      Instr (Sbci, RegInt(25, 0xFF));
		     ]
	    | "not" -> [Instr (Sbiw, RegInt(24, 0));
			Instr (Brne, String "1f");
			Instr (Ldi, RegInt (24, 1));
			Instr (Rjmp, String "2f");
			Label "1";
			Instr (Clr, Reg 24);
			Label "2";
                        Instr (Clr, Reg 25);
		       ]
	    | "~" -> [Instr (Com, Reg(25));
		      Instr (Com, Reg(24));
		     ]
	    | _ -> []
	end
  | Fcall (id, el, c, _) ->
    if !c then
      begin
        let pop_arg i _ =
          match i with
          | 0 -> []
          | 1 ->
            [Instr (Pop, Reg(23));
	     Instr (Pop, Reg(22))]
          | 2 ->
            [Instr (Pop, Reg(21));
	     Instr (Pop, Reg(20))]
          | 3 ->
            [Instr (Pop, Reg(19));
	     Instr (Pop, Reg(18))]
          | 4 ->
            [Instr (Pop, Reg(17));
	     Instr (Pop, Reg(16))]
          | 5 ->
            [Instr (Pop, Reg(15));
	     Instr (Pop, Reg(14))]
          | 6 ->
            [Instr (Pop, Reg(13));
	     Instr (Pop, Reg(12))]
          | 7 ->
            [Instr (Pop, Reg(11));
	     Instr (Pop, Reg(10))]
          | 8 ->
            [Instr (Pop, Reg(9));
	     Instr (Pop, Reg(8))]
          | _ ->
            [Instr (Pop, Reg(0));
	     Instr (Pop, Reg(0))]
        in
        let rec push_arg = function
          | [] -> []
          | h::[] -> [p_expr mem h]
          | h::t ->
            List.concat [p_expr mem h;
                         [Instr (Push, Reg(24));
	                  Instr (Push, Reg(25))]]::push_arg t
        in
        List.concat [List.concat (push_arg (List.rev el));
                     List.concat (mapi pop_arg el);
                     [Instr (Call, String id)]
                    ]
      end
    else
      begin
        List.concat
          [[Instr (Push, Reg(29));
            Instr (Push, Reg(28))];
           (List.concat
              (List.map
	         (fun x ->
	           p_expr mem x@
	             [Instr (Push, Reg(25));
	              Instr (Push, Reg(24))])
	         el));
           begin
             try
               let _ = Hashtbl.find mem.Mem.loc id in
               [MemLoad (id, mem);
                Instr (Movw, RegReg(30, 24));
                Instr (Icall, NoneOp)]
             with Not_found ->
               [Instr
                   (Call, String id)]
           end;
           (List.concat
	      (List.map
	         (fun _ ->
	           [Instr (Pop, Reg(0));
	            Instr (Pop, Reg(0))])
	         el));
           [Instr (Pop, Reg 28);
            Instr (Pop, Reg 29)]]
      end
  | Mcall (e, m, el, ty, loc) ->
    let ty = !ty in
    p_expr mem (Fcall (ty ^ "_" ^ m, e::el, ref false, loc))
  | New (cl, el, loc) ->
    let size = (Hashtbl.find cl_mem cl).Mem.size in
    [Instr (Ldi, RegInt(24, size));
     Instr (Ldi, RegInt(25, 0));
     Instr (Call, String("malloc"));
     Instr (Push, Reg(28));
     Instr (Push, Reg(29));
     Instr (Push, Reg(25));
     Instr (Push, Reg(24))]@
      (List.concat
	 (List.map
	    (fun x ->
	       p_expr mem x@
		 [Instr (Push, Reg(25));
		    Instr (Push, Reg(24))])
	    el))@
      [Instr (Call, String(cl ^ "_init"))]@
      (List.concat
	 (List.map
	    (fun _ ->
	      [Instr (Pop, Reg(0));
	       Instr (Pop, Reg(0))])
	    el))@
      [Instr (Pop, Reg(0));
       Instr (Pop, Reg(0));
       Instr (Pop, Reg 29);
       Instr (Pop, Reg 28)]
  | Asm (eo, a, _) ->
    begin
      match eo with
      | None -> []
      | Some e -> p_expr mem e
    end@
      [Comment "Inline ASM";
       Code a;
       Comment "end"]
  | Tern (c, e1, e2, _) ->
      label := !label + 3;
      let lbl = !label in
      List.concat
        [p_expr mem c;
	 [Instr (Cp, RegReg(24, 1));
	  Instr (Cpc, RegReg(25, 1));
	  Instr (Brne,
		 String (gbl_lbl (lbl-2)));
          Instr (Rjmp,
                 String (gbl_lbl (lbl-1)));
          Label (gbl_lbl (lbl-2))
	 ];
	 p_expr mem e1;
	 [Instr (Rjmp,
		 String (gbl_lbl (lbl-3)));
	  Label (gbl_lbl (lbl-1))];
	 p_expr mem e2;
	 [Label (gbl_lbl (lbl-3))]
        ]

let rec p_instr mem = function
  | Let (v, e, _) ->
      Mem.add mem v;
      List.concat [p_expr mem e;
                   [MemStore (v, mem)]]
  | Affect (le, re, _) ->
        begin
	  match le with
	  | Var (v, _) ->
            List.concat [p_expr mem re;
                         [MemStore (v, mem)]]
          | Addr (a, _) ->
            List.concat [p_expr mem re;
                         [Instr (Sts,
                                 match a with
                                 | AString s  -> StringReg(s, 24)
                                 | ADString (_, s)  -> StringReg(s, 24)
                                 | AInt i -> IntReg(i, 24)
                                 | ADInt (_, l) -> IntReg(l, 24));
                         ];
                         (match a with
                         | ADString (s, _) -> [Instr (Sts, StringReg(s, 25))]
                         | ADInt (h, _) -> [Instr (Sts, IntReg(h, 25))]
                         | _ -> [])
                        ]
          | Get (id, e, size, mc, _) ->
            begin
              match !mc with
              | None ->
                List.concat [p_expr mem id;
                             [Instr (Push, Reg(24));
                              Instr (Push, Reg(25))];
                             p_expr mem e;
                             begin
                               match !size with
                               | 1 -> []
                               | 2 -> [Instr (Adiw, RegInt(24, 1));
                                       Instr (Lsl, Reg 24);
		                       Instr (Rol, Reg 25)]
                               | _ -> failwith "gen get"
                             end;
                             [Instr (Pop, Reg(31));
                              Instr (Pop, Reg(30));
                              Instr (Add, RegReg(30, 24));
                              Instr (Adc, RegReg(31, 25));
                              Instr (Push, Reg(30));
                              Instr (Push, Reg(31))];
                             p_expr mem re;
                             [Instr (Pop, Reg(31));
                              Instr (Pop, Reg(30))];
                             match !size with
                             | 1 ->
                               [Instr (St, StringReg("Z", 24))]
                             | 2 ->
                               [Instr (St, StringReg("Z", 24));
                                Instr (Std, StringReg("Z+1", 25));]
                             | _ -> failwith "gen affect get"
                            ]
              | Some e ->
                begin
                  match e with
                  | Mcall (e, m, el, ty, loc) ->
                    List.concat
                      [[Instr (Push, Reg(28));
                        Instr (Push, Reg(29))];
                       p_expr mem e;
                       [Instr (Push, Reg(25));
	                Instr (Push, Reg(24))];
                       p_expr mem (List.hd el);
                       [Instr (Push, Reg(25));
	                Instr (Push, Reg(24))];
                       p_expr mem re;
                       [Instr (Push, Reg(25));
	                Instr (Push, Reg(24));
                        Instr (Call, String (!ty ^ "_" ^ m))];
                       (List.concat
	                  (List.map
	                     (fun _ ->
	                       [Instr (Pop, Reg(0));
	                        Instr (Pop, Reg(0))])
	                     [1; 2; 3]));
                       [Instr (Pop, Reg 29);
                        Instr (Pop, Reg 28)]]
                  | _ -> failwith "gen affect get"
                end
            end
	  | Field (e, f, ty, _) ->
            List.concat [p_expr mem re;
	                 [Instr (Movw, RegReg(18, 24))];
                         begin
                           let ty = !ty in
                           let loc =
                             (Hashtbl.find
	                        (Hashtbl.find cl_mem ty).Mem.loc f) - 1
                           in
                           List.concat [p_expr mem e;
                                        [Instr (Adiw, RegInt (24, loc));
                                         Instr (Movw, RegReg(30, 24));
	                                 Instr (St, StringReg("Z", 18));
	                                 Instr (Std, StringReg("Z+1", 19))]]
                         end
                        ]
	  | _ -> failwith "gen affect"
        end
  | Return (e, _) ->
      List.concat
	[p_expr mem e;
	 [Instr (Rjmp, String "0f")]]
  | If (e, b, _) ->
      label := !label + 2;
      let lbl = !label in
      List.concat
        [p_expr mem e;
	 [Instr (Cp, RegReg(24, 1));
	  Instr (Cpc, RegReg(25, 1));
	  Instr (Brne,
		 String (gbl_lbl (lbl-2)));
          Instr (Rjmp,
                 String (gbl_lbl (lbl-1)));
          Label (gbl_lbl (lbl-2))
	 ];
	 p_body mem b;
	 [Label (gbl_lbl (lbl-1))]
        ]
  | IfElse (e, b1, b2, _) ->
      label := !label + 3;
      let lbl = !label in
      List.concat
        [p_expr mem e;
	 [Instr (Cp, RegReg(24, 1));
	  Instr (Cpc, RegReg(25, 1));
	  Instr (Brne,
		 String (gbl_lbl (lbl-2)));
          Instr (Rjmp,
                 String (gbl_lbl (lbl-1)));
          Label (gbl_lbl (lbl-2))
	 ];
	 p_body mem b1;
	 [Instr (Rjmp,
                 String (gbl_lbl (lbl-3)));
	  Label (gbl_lbl (lbl-1))];
	 p_body mem b2;
	 [Label (gbl_lbl (lbl-3))]
        ]
  | While (e, b, _) ->
      label := !label + 3;
      let lbl = !label in
      List.concat
        [[Label (gbl_lbl (lbl-3))];
	 p_expr mem e;
	 [Instr (Cp, RegReg(24, 1));
	  Instr (Cpc, RegReg(25, 1));
	  Instr (Brne,
		 String (gbl_lbl (lbl-2)));
          Instr (Rjmp,
                 String (gbl_lbl (lbl-1)));
          Label (gbl_lbl (lbl-2))
	 ];
	 p_body mem b;
	 [Instr (Rjmp,
                 String (gbl_lbl (lbl-3)));
	  Label (gbl_lbl (lbl-1))]
        ]
  | For (inc, i, init, e, b, loc) ->
    let i2 = "_" ^ i in
    p_body mem
      [Let (i2, e, loc);
       Let (i, init, loc);
       While (BinOp ((if inc then "<=" else ">="),
                     Var (i, loc), Var (i2, loc), loc),
              b@[Affect (Var (i, loc),
                         BinOp ((if inc then "+" else "-"),
                                Var(i, loc), Int(1, loc), loc),
                         loc)],
              loc)]
  | Expr (e, _) ->
      p_expr mem e
  | Asminstr (s, _) -> [Code s]
  | Del (e, destr, loc) ->
    List.concat
      [p_expr mem e;
       (match !destr with
       | None -> []
       | Some s ->
         p_expr mem
           (Mcall (e, "destr", [], ref s, loc)));
       [Instr (Call, String("free"))]]

and p_body mem b =
  List.concat (List.map (p_instr mem) b)

let p_isr mem b =
  List.concat [[Instr (Push, Reg(1));
                Instr (Push, Reg(0));
                Instr (In, RegString(0, "__SREG__"));
                Instr (Push, Reg(0));
                Instr (Eor, RegReg(1, 1));
                Instr (Push, Reg(18));
                Instr (Push, Reg(19));
                Instr (Push, Reg(20));
                Instr (Push, Reg(21));
                Instr (Push, Reg(22));
                Instr (Push, Reg(23));
                Instr (Push, Reg(24));
                Instr (Push, Reg(25));
                Instr (Push, Reg(26));
                Instr (Push, Reg(27));
                Instr (Push, Reg(28));
                Instr (Push, Reg(29));
                Instr (Push, Reg(30));
                Instr (Push, Reg(31));
                Putfb mem];
               p_body mem b;
               [Instr (Pop, Reg(31));
                Instr (Pop, Reg(30));
                Instr (Pop, Reg(29));
                Instr (Pop, Reg(28));
                Instr (Pop, Reg(27));
                Instr (Pop, Reg(26));
                Instr (Pop, Reg(25));
                Instr (Pop, Reg(24));
                Instr (Pop, Reg(23));
                Instr (Pop, Reg(22));
                Instr (Pop, Reg(21));
                Instr (Pop, Reg(20));
                Instr (Pop, Reg(19));
                Instr (Pop, Reg(18));
                Instr (Pop, Reg(0));
                Instr (Out, StringReg("__SREG__", 0));
                Instr (Pop, Reg(0));
                Instr (Pop, Reg(1));
                Instr (Reti, NoneOp)]]

let p_func ?idc:(idc=None) gbl id al b =
  let m =
    begin
      match idc with
      | None -> Hashtbl.find gbl.Env.func id
      | Some idc ->
        let cl = Hashtbl.find gbl.Env.cl idc in
        Hashtbl.find cl.Class.m id
    end
  in
  if m.Class.used then
    begin
      let ty = Hashtbl.find gbl.Env.local
        (match idc with
        | None -> id
        | Some idc -> idc ^ "_" ^ id)
      in
      let mem = Mem.create (Some ty) in
      begin
        match idc with
        | None -> ()
        | Some idc ->
          Mem.add mem "self";
          Hashtbl.add ty "self" (Typing.Tid idc)
      end;
      let _ = List.iter
	(Mem.add mem) al in
      Mem.add mem "~call";
      let _ = Hashtbl.iter
	(fun s _ ->
	  if not(List.mem s al) then
	    Mem.add mem s) ty in
      let offset =
        (match idc with
        | None -> 2
        | Some _ -> 4) +
          (List.length al) * 2
      in
      List.concat [[Label
                       (match idc with
                       | None ->  id
                       | Some idc -> idc ^ "_" ^ id)];
                   [MemAlloc (mem, offset)];
                   p_body mem b;
	           [Label "0";
                    MemFree (mem, offset);
                    Instr (Ret, NoneOp)]]
    end
  else []

let p_cl gbl idc label mem cl =
  let p_cldef = function
    | Vdef (_, v, _) -> Mem.add mem v; []
    | Fdef ((id, al, b), _) ->
      p_func ~idc:(Some idc) gbl id
        (List.map (fun (_, a) -> a) al) b
  in
  List.concat (List.map p_cldef cl)

let g_body gbl =
  let main_mem = Mem.create
    (Some (Hashtbl.find gbl.Env.local "_main"))
  in
  function
  | Setup (b, loc) ->
    List.concat [[Dir ("globl", "main");
                  Label "main";
                  MemAlloc (main_mem, 0)];
                 p_body main_mem b;
                 [Instr (Rjmp, String "_loop")]]
  | Loop (b, loc) ->
    List.concat [[Label "_loop"];
                 p_body main_mem b;
                 [Instr (Rjmp, String "_loop")]]
  | Func ((id, al, b), loc) ->
    p_func gbl id (List.map (fun (_, a) -> a) al) b
  | ISR (id, b, loc) ->
    let id = id ^ "_vect" in
    Dir ("globl", id)::
      (Label (id))::
      (p_isr main_mem b)
  | Class (id, mother, classdef, loc) ->
    let mem = match mother with
      | None -> Mem.create None
      | Some mother -> Mem.copy (Hashtbl.find cl_mem mother)
    in
    Hashtbl.add cl_mem id mem;
    p_cl gbl id label mem classdef
  | Using _ -> []
  | FSM (_, _, _) -> failwith "g_body fsm"
  | Extern (_, _, _) -> []

let gen gbl cout =
  let l = ref [] in
  header cout;
  while not (Queue.is_empty gbl.Env.ast) do
    let id, ast = Queue.take gbl.Env.ast in
    l := List.concat [List.map (g_body gbl) ast;
                      !l]
  done;
  let c_static = p_static () in
  print_asm cout (List.concat [c_static;
                               [Dir ("text", "")];
                               List.concat !l;
			       if List.length c_static > 0 then
			         [Dir ("globl", "__do_copy_data")]
			       else []]);
