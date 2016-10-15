(*
** class.ml for Mara
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
** Started on  Tue Dec 25 13:06:52 2012 Pierre Surply
**  Last update Sat Aug 24 15:04:34 2013 Pierre Surply
*)

open Typing

type m =
    {
      mutable arg	: (string * type_name) list;
      mutable ret	: type_name;
      mutable used      : bool;
      mutable c         : bool;
    }

type t =
    {
      id                : string;
      mutable ty        : string list;
      prop      	: (string, type_name) Hashtbl.t;
      m         	: (string, m) Hashtbl.t;
      mutable mother    : t option
    }

let create id mother =
  {
    id = id;
    ty = [];
    prop = Hashtbl.create 11;
    m = Hashtbl.create 11;
    mother = mother;
  }

let create_m () =
  {
    arg = [];
    ret = Tnone;
    used = false;
    c = false;
  }

exception Diff
exception Cannot_Match of string * string * string *
    (Lexing.position * Lexing.position)
exception Ambiguous_typing of string * string * string * string *
    (Lexing.position * Lexing.position)

let constrain id_func id_var loc classes sign =
  let match_with cl =
    Hashtbl.iter
      (fun id (idcl, ty) ->
        let rec find_field cl =
          try
            let prop = Hashtbl.find cl.prop id in
            Queue.iter (fun x -> !x := cl.id) idcl;
            prop
          with Not_found ->
            begin
              match cl.mother with
              | None -> raise Diff
              | Some mother -> find_field mother
            end
        in
        let ty_prop = find_field cl in
        ignore (cmp_type loc (ty_prop, ty)))
      sign.Typing.prop;
    Hashtbl.iter
      (fun id (idcl, args, ty) ->
        try
          let rec find_method cl =
            try
              let m = Hashtbl.find cl.m id in
              Queue.iter (fun x -> !x := cl.id) idcl;
              m
            with Not_found ->
              begin
                match cl.mother with
                | None -> raise Diff
                | Some mother -> find_method mother
              end
          in
          let m = find_method cl in
          if not (List.for_all2
                    (function (_, a) ->
                      function b ->
                        try
                          ignore (cmp_type loc (a, b)); true
                        with Bad_type (_, _, _) -> false)
                    m.arg args)
          then
            raise Diff
          else
            m.used <- true
        with Not_found -> raise Diff)
      sign.Typing.m
  in
  let matched = Queue.create () in
  Hashtbl.iter
    (fun id cl ->
      try
        match_with cl;
        Queue.add cl matched
      with Diff -> ()) classes;
  match Queue.length matched with
  | 0 ->
    raise (Cannot_Match (sign2str sign,
                         id_var, id_func, loc))
  | 1 ->
    let cl = Queue.take matched in
    let rec find_destr cl =
      try
        let _ = Hashtbl.find cl.m "destr" in
        Queue.iter (fun x -> !x := Some cl.id) sign.destr
      with Not_found ->
        begin
          match cl.mother with
          | None -> Queue.iter (fun x -> !x := None) sign.destr
          | Some mother -> find_destr mother
        end
    in
    find_destr cl;
    Tid cl.id
  | _ ->
    let classes = ref [] in
    while not (Queue.is_empty matched) do
      classes := ("* " ^ (Queue.take matched).id)::!classes
    done;
    let s = String.concat "\n\t" !classes in
    raise (Ambiguous_typing (sign2str sign, s, id_var, id_func, loc))
