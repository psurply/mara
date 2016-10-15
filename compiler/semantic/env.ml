(*
** env.ml for Mara
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
** Started on  Tue Nov 20 18:16:42 2012 Pierre Surply
**  Last update Wed Jul 17 14:41:50 2013 Pierre Surply
*)

open Typing

type local_env = (string, type_name) Hashtbl.t

type globl_env =
    {
      ast               :
	(string * ((Lexing.position * Lexing.position) Ast.body) list) Queue.t;
      cl                : (string, Class.t) Hashtbl.t;
      mutable st_cl	: string list;
      local             : (string, local_env) Hashtbl.t;
      func              : (string, Class.m) Hashtbl.t
    }

let create_globl () =
  {
    ast = Queue.create ();
    cl = Hashtbl.create 11;
    st_cl = [];
    local = Hashtbl.create 13;
    func = Hashtbl.create 13;
  }

let create_local () = Hashtbl.create 11

let copy env = (Hashtbl.copy env : local_env)
