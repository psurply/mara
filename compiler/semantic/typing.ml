(*
** typing.ml for Mara
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
** Started on  Tue Nov 20 18:03:09 2012 Pierre Surply
**  Last update Wed Aug 21 18:35:12 2013 Pierre Surply
*)

open Ast

type 'a sign =
{
  prop          : (string, ((string ref ref) Queue.t * 'a)) Hashtbl.t;
  m             : (string, ((string ref ref) Queue.t *
                               ('a list) * 'a)) Hashtbl.t;
  destr         : (string option ref ref) Queue.t
}

type type_name =
  | Tnone
  | Tint
  | Tstring
  | Tvect
  | Tid of string
  | Tnoconstr of type_name sign
  | Tfunc of type_name list * type_name

exception Bad_type of type_name * type_name *
    (Lexing.position * Lexing.position)

let rec cmp_type loc = function
  | (t1, t2) when t1 = t2 -> t1
  | (Tnone, t) | (t, Tnone) -> t
  | (Tfunc (arg1, ret1), Tfunc (arg2, ret2)) ->
    begin
      try
        Tfunc (List.map2 (fun a b -> cmp_type loc (a, b)) arg1 arg2,
               cmp_type loc (ret1, ret2))
      with Invalid_argument _ ->
        raise (Bad_type (Tfunc (arg1, ret1), Tfunc (arg2, ret2), loc))
    end
  | (t1, t2) -> raise (Bad_type (t1, t2, loc))

let rec type2str = function
  | Tnone               -> "undef"
  | Tint                -> "integer"
  | Tstring             -> "cstring"
  | Tvect               -> "vect"
  | Tid s               -> s
  | Tnoconstr s         -> sign2str s
  | Tfunc (arg, ty)     -> func2str arg ty

and func2str arg ty =
  let s = ref "(" in
  begin
    match arg with
    | [] -> s := !s ^ (Printf.sprintf "void -> ")
    | _ ->
      List.iter
        (fun ty -> s := !s ^ (Printf.sprintf "%s -> " (type2str ty)))
        arg;
  end;
  !s ^ (Printf.sprintf "%s" (type2str ty)) ^ ")"

and env2strl env =
  let l = ref [] in
  Hashtbl.iter
    (fun id ty -> l := (Printf.sprintf "%s: %s" id (type2str ty))::!l)
    env;
  !l

and sign2str sign =
  let s = ref "<\n" in
  Hashtbl.iter
    (fun id (_, ty) ->
      s := !s ^ (Printf.sprintf "\t%s : %s\n" id (type2str ty))) sign.prop;
  Hashtbl.iter
    (fun id (_, arg, ty) ->
      s := !s ^ (Printf.sprintf "\t%s : " id) ^ (func2str arg ty) ^ "\n")
    sign.m;
  !s ^ ">"

let rec list2type l =
  let str2type = function
    | "undef"      -> Tnone
    | "integer"    -> Tint
    | "cstring"    -> Tstring
    | "vect"       -> Tvect
    | s            -> Tid s
  in
  let rec build_func = function
    | h::[] -> ([], str2type h)
    | "void"::t ->
      let arg, ret = build_func t in
      (arg, ret)
    | h::t ->
      let arg, ret = build_func t in
      ((str2type h)::arg, ret)
    | _ -> failwith "build_func"
  in
  if List.length l = 1 then
    str2type (List.hd l)
  else
    let arg, ret = build_func l in
    Tfunc (arg, ret)

let create_sig () =
{
  prop = Hashtbl.create 13;
  m = Hashtbl.create 13;
  destr = Queue.create ();
}
