(*
** opt_expr.ml for Mara
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
** Started on  Thu Jan  3 12:03:59 2013 Pierre Surply
**  Last update Mon Aug 19 17:16:08 2013 Pierre Surply
*)

open Ast
open Typing

let opt_uniop i = function
  | "-"		-> -i
  | "not"	-> if i > 0 then 0 else 1
  | "~"		-> (lnot i) land 0xFF
  | _		-> failwith "opt_uniop"

let opt_binop l r = function
  | "+"		-> l + r
  | "-"		-> l + r
  | "*"		-> l * r
  | "/"		-> l / r
  | "%"		-> l mod r
  | "|"		-> (l lor r) land 0xFF
  | "&"		-> (l land r) land 0xFF
  | "^"		-> (l lxor r) land 0xFF
  | "<<"	-> (l lsl r) land 0xFF
  | ">>"	-> (l lsr r) land 0xFF
  | "or"	-> if l > 0 || r > 0 then 1 else 0
  | "and"	-> if l > 0 && r > 0 then 1 else 0
  | ">"		-> if l > r then 1 else 0
  | "<"		-> if l < r then 1 else 0
  | ">="	-> if l >= r then 1 else 0
  | "<="	-> if l <= r then 1 else 0
  | "="		-> if l = r then 1 else 0
  | "<>"	-> if l != r then 1 else 0
  | _		-> failwith "opt_binop"

let rec opt_expr local = function
  | Cast (_, e, _) -> e
  | ENone _ as n -> n
  | Int _ as i -> i
  | Addr _ as a -> a
  | Const _ as c -> c
  | Str _ as s -> s
  | Vect (el, loc) ->
    Vect (List.map (opt_expr local) el, loc)
  | Var _ as v -> v
  | Field _ as f -> f
  | Get (id, e, size, mcall, loc) ->
    begin
      match !mcall with
      | None -> Get(opt_expr local id, opt_expr local e, size, ref None, loc)
      | Some mcall -> Get(id, e, size, ref (Some (opt_expr local mcall)), loc)
    end
  | BinOp (op, le, re, loc) ->
      begin
	match (opt_expr local le, opt_expr local re) with
	  | (Int (l, _),
	     Int (r, _))	-> Int (opt_binop l r op, loc)
	  | (ole, ore)		-> BinOp (op, ole, ore, loc)
      end
  | UniOp (op, e, loc) ->
      begin
	match opt_expr local e with
	  | Int (i, _)	-> Int (opt_uniop i op, loc)
	  | oe		-> UniOp (op, oe, loc)
      end
  | Fcall (id, el, c, loc) ->
    Fcall (id,
	   List.map (opt_expr local) el,
           c, loc)
  | Mcall (e, id, el, ty, loc) ->
    Mcall (opt_expr local e, id,
	   List.map (opt_expr local) el,
           ty,
	   loc)
  | New (cl, el, loc) ->
      New (cl,
	  List.map (opt_expr local) el,
	  loc)
  | Tern (c, e1, e2, loc) ->
    Tern (opt_expr local c,
	  opt_expr local e1,
	  opt_expr local e2, loc)
  | Asm (oe, s, loc) as a ->
    begin
      match oe with
      | None -> a
      | Some e ->
	Asm (Some(opt_expr local e), s, loc)
    end
  | Lambda (vl, b, name, loc) ->
    Lambda (vl, b, name, loc)
