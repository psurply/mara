(*
** sem.ml for Mara
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
** Started on  Tue Dec 25 12:49:26 2012 Pierre Surply
**  Last update Wed Aug 21 17:25:00 2013 Pierre Surply
*)

open Ast
open Typing
open Conf
open Opt_expr

exception Cannot_find_class of string * (Lexing.position * Lexing.position)
exception Undeclared_var of string * (Lexing.position * Lexing.position)
exception Unknown_field of string * (Lexing.position * Lexing.position)
exception Unknown_method of string * (Lexing.position * Lexing.position)
exception Unknown_function of string * (Lexing.position * Lexing.position)
exception Not_an_object of (Lexing.position * Lexing.position)
exception Not_a_function of string * (Lexing.position * Lexing.position)
exception Given_arg of string * int * int * (Lexing.position * Lexing.position)
exception No_init of string * (Lexing.position * Lexing.position)
exception Ret_nometh of (Lexing.position * Lexing.position)

let lambda_id = ref 0

let main_env = Env.create_local ()

let rec get_type_expr
    ?affect:(affect=false) ?fty:(fty=Tnone) cl fix_ty globl local e =
  let rec check_arg id loc vars n = function
    | ([], []) -> ()
    | ([], l) ->
	raise (Given_arg (id, n, n + List.length l, loc))
    | (l, []) ->
	raise (Given_arg (id, n + List.length l, n, loc))
    | (e1::t1, (_, ty)::t2) ->
	let gt = get_type_expr ~fty:ty cl true globl local e1 in
	let _ = cmp_type loc (ty, gt) in
	  check_arg id loc vars (n+1) (t1, t2)
  and rget_type_expr ?vars:(vars=None) globl local = function
    | Cast (ty, e, loc) ->
      let _ = rget_type_expr
	~vars:vars globl local e
      in
      list2type ty
    | ENone _ -> Tnone
    | Int (_, _) -> Tint
    | Str (_, _) -> Tstring
    | Vect (el, _) ->
      List.iter
        (fun x -> let _ = get_type_expr cl true globl local x in ())
        el;
      Tvect
    | Addr (_, _) -> Tint
    | Const (_, _) -> Tnone
    | Var (id, loc) ->
	begin
	  try
	    let ty = Hashtbl.find local id in
	      begin
		match vars with
		  | None -> ()
		  | Some vars ->
		      if ty = Tnone then
			Queue.push id vars
	      end;
	      ty
	  with Not_found ->
	    match (id, cl) with
	    | ("self", Some cl) -> Tid (cl)
	    | _ ->
              begin
                try
                  let f = Hashtbl.find globl.Env.func id in
                  f.Class.used <- true;
                  Tfunc (List.map (fun (_, ty) -> ty) f.Class.arg, f.Class.ret)
                with Not_found ->
                  raise (Undeclared_var (id, loc))
              end
	end
    | Get (id, e, size, mc, loc) ->
      let _ = cmp_type loc (Tint,
			    rget_type_expr
			      ~vars:vars globl local e)
      in
      begin
        try
          let mcall = Mcall
            (id, (if affect then "set" else "get"),
             (if affect then [e; ENone loc] else [e]), ref "", loc)
          in
          let ty = rget_type_expr ~vars:vars globl local mcall in
          mc := Some mcall;
          ty
        with
          Not_an_object _ ->
            begin
              match get_type_expr cl false globl local id with
              | Tstring ->
                let _ = get_type_expr ~fty:Tstring cl true globl local id in
                size := 1;
                Tint
              | Tvect ->
                let _ = get_type_expr ~fty:Tvect cl true globl local id in
                size := 2;
                Tnone
              | _ -> raise (Not_an_object loc)
            end
      end
    | Field (e, prop, ty, loc) ->
      let clvars = Queue.create () in
      let cl = rget_type_expr ~vars:(Some clvars) globl local e in
      begin
        match cl with
        | Tid s ->
          begin
            let rec find_field cl =
              try
                let p = Hashtbl.find cl.Class.prop prop in
                ty := cl.Class.id;
                p
              with Not_found ->
                begin
                  match cl.Class.mother with
                  | None ->
                    raise (Unknown_field (prop, loc))
                  | Some mother -> find_field mother
                end
            in
            let cl =
              try
                Hashtbl.find globl.Env.cl s
              with Not_found ->
                raise (Cannot_find_class (s, loc))
            in
            find_field cl
          end
        | Tnone ->
          let sign = create_sig () in
          let q = Queue.create () in
          Queue.add (ref ty) q;
          Hashtbl.add sign.prop prop (q, Tnone);
          Queue.iter
	    (fun x -> Hashtbl.replace local x
              (Tnoconstr sign)) clvars;
          Tnone
        | Tnoconstr sign ->
          begin
            try
              let q, t = Hashtbl.find sign.prop prop in
              Queue.add (ref ty) q; t
            with Not_found ->
              let q = Queue.create () in
              Queue.add (ref ty) q;
              Hashtbl.add sign.prop prop (q, Tnone);
              Tnone
          end
        | _ -> raise (Not_an_object loc)
      end
    | BinOp (_, le, re, loc) ->
	let _ = cmp_type loc (Tint,
			      rget_type_expr
				~vars:vars globl local le)
	in
	let _ = cmp_type loc (Tint,
			      rget_type_expr
				~vars:vars globl local re)
	in Tint
    | UniOp (_, e, loc) ->
	cmp_type loc (Tint,
		      rget_type_expr ~vars:vars globl local e)
    | Fcall (id, el, c, loc) ->
      begin
        try
          let f = Hashtbl.find globl.Env.func id in
          check_arg id loc vars 0 (el, f.Class.arg);
          f.Class.used <- true;
          c := f.Class.c;
          f.Class.ret
        with
          Not_found ->
            begin
              try
                match Hashtbl.find local id with
                | Tfunc (arg, ret) ->
                  check_arg id loc vars 0 (el,
                                           List.map (fun x -> ("", x))
                                             arg);
                  ret
                | Tnone ->
                  Hashtbl.replace local id
                    (Tfunc (List.map
                              (fun x -> get_type_expr cl false globl local x)
                              el,
                            Tnone));
                  Tnone
                | _ -> raise (Not_a_function (id, loc))
              with Not_found ->
                raise (Unknown_function (id, loc))
            end
      end
    | Mcall (e, id, el, ty, loc) ->
      let clvars = Queue.create () in
      let cl = rget_type_expr ~vars:(Some clvars) globl local e in
      begin
        match cl with
        | Tid s ->
          begin
            let rec find_method cl =
              try
                let m = Hashtbl.find cl.Class.m id in
                ty := cl.Class.id;
                m
              with Not_found ->
                begin
                  match cl.Class.mother with
                  | None ->
                    raise (Unknown_method (id, loc))
                  | Some mother -> find_method mother
                end
            in
            let cl =
              try
                Hashtbl.find globl.Env.cl s
              with Not_found ->
                raise (Cannot_find_class (s, loc))
            in
            let m = find_method cl in
	    check_arg id
	      loc vars 0 (el, m.Class.arg);
            m.Class.used <- true;
	    m.Class.ret
          end
        | Tnone ->
          let sign = create_sig () in
          let q = Queue.create () in
          Queue.add (ref ty) q;
          Hashtbl.add sign.m id
            (q, List.map (function e ->
              rget_type_expr globl local e)
               el, Tnone);
          Queue.iter
	    (fun x -> Hashtbl.replace local x
              (Tnoconstr sign)) clvars;
          Tnone
        | Tnoconstr sign ->
          begin
            try
              let q, cargs, cty = Hashtbl.find sign.m id in
              check_arg id
		loc vars 0 (el, List.map
                  (function a -> ("", a)) cargs);
              Queue.add (ref ty) q;
              cty
            with Not_found ->
              let q = Queue.create () in
              Queue.add (ref ty) q;
              Hashtbl.add sign.m id
                (q, List.map (function e ->
                  rget_type_expr globl local e)
                   el, Tnone); Tnone
          end
        | _ -> raise (Not_an_object loc)
      end
    | New (cl, el, loc) ->
	begin
	  try
	    let _ = Hashtbl.find globl.Env.cl cl in ()
	  with Not_found -> raise (Cannot_find_class (cl, loc))
	end;
	let cla = Hashtbl.find globl.Env.cl cl in
	  begin
	    try
	      let m = Hashtbl.find cla.Class.m "init" in
              m.Class.used <- true;
	      check_arg (cl ^ ".init")
		loc vars 0 (el, m.Class.arg)
	    with Not_found -> raise (No_init (cl, loc))
	  end;
	Tid cl
    | Tern (c, e1, e2, loc) ->
      let _ = get_type_expr cl fix_ty globl local c in
      let t1 = get_type_expr cl fix_ty globl local e1 in
      let t2 = get_type_expr cl fix_ty globl local e2 in
      cmp_type loc (t1, t2)
    | Asm (oe, _, _) ->
      begin
	match oe with
	| None -> ()
	| Some e ->
	  let _ = get_type_expr cl fix_ty globl local e in ()
      end;
      Tnone
    | Lambda (vl, b, name, loc) ->
      let id = !lambda_id in
      lambda_id := !lambda_id + 1;
      name := "__lambda" ^ (string_of_int id);
      let m, oil, env = add_function !name globl vl !b loc in
      b := oil;
      m.Class.used <- true;
      Hashtbl.add globl.Env.local (!name) env;
      Hashtbl.replace globl.Env.func !name m;
      Queue.add (!name, [Func((!name, vl, !b), loc)])
        globl.Env.ast;
      Tfunc (List.map (fun (_, x) -> x) m.Class.arg, m.Class.ret)
  in
    if not fix_ty then
      rget_type_expr globl local e
    else
      begin
	let vars = Queue.create () in
	let ty =
          if fty = Tnone then
            rget_type_expr ~vars:(Some vars) globl local e
          else
            match rget_type_expr ~vars:(Some vars) globl local e with
            | Tnone -> fty
            | t -> t
        in
	  Queue.iter
	    (fun x -> Hashtbl.replace local x ty) vars;
	  ty
      end

and check_bloc ?cl:(cl=None) ?mth:(mth=None)
    ?fix_ty:(fix_ty=false) globl local body =
  let check = function
    | Let (id, e, loc) ->
      begin
	try
	  let _ = Hashtbl.find local id in
	  Affect (Var (id, loc), opt_expr local e, loc)
	with Not_found ->
	  Hashtbl.add local id
	    (get_type_expr cl fix_ty globl local e);
	  Let (id, opt_expr local e, loc)
      end
    | Affect (le, re, loc) ->
      let rt = get_type_expr cl fix_ty globl local re in
      let lt = get_type_expr ~affect:true ~fty:rt cl true globl local le in
      begin
        if rt = Tnone then
          let _ = get_type_expr ~fty:lt cl fix_ty globl local re in ()
        else
          let _ = cmp_type loc (lt, rt) in ()
      end;
      Affect (opt_expr local le, opt_expr local re, loc)
    | Return (e, loc) ->
      begin
        match mth with
        | Some m ->
          let t = get_type_expr cl fix_ty globl local e in
          let _ = cmp_type loc (m.Class.ret, t) in
          m.Class.ret <- t;
	  Return (opt_expr local e, loc)
        | _ -> raise (Ret_nometh loc)
      end
    | If (e, b, loc) ->
	let _ = get_type_expr cl fix_ty globl local e in
	let ob = check_bloc ~cl:cl ~mth:mth ~fix_ty:fix_ty globl local b in
	  If (opt_expr local e, ob, loc)
    | While (e, b, loc) ->
      let _ =  get_type_expr cl fix_ty globl local e in
      let ob = check_bloc ~cl:cl ~mth:mth ~fix_ty:fix_ty globl local b in
	While (opt_expr local e, ob, loc)
    | IfElse (e, b1, b2, loc) ->
      let _ = get_type_expr cl fix_ty globl local e in
      let ob1 = check_bloc ~cl:cl ~mth:mth ~fix_ty:fix_ty globl local b1 in
      let ob2 = check_bloc ~cl:cl ~mth:mth ~fix_ty:fix_ty globl local b2 in
	IfElse (opt_expr local e, ob1, ob2, loc)
    | For (up, i, init, cond, b, loc) ->
      let _ = get_type_expr cl fix_ty globl local cond in
      Hashtbl.add local i
	(cmp_type loc
	   (Tint,
	    get_type_expr ~fty:Tint cl true globl local init));
      Hashtbl.add local ("_" ^ i)
	(cmp_type loc
           (Tint,
            get_type_expr ~fty:Tint cl true globl local cond));
      let ob = check_bloc ~cl:cl ~mth:mth ~fix_ty:fix_ty globl local b in
      For (up, i, opt_expr local init,
	   opt_expr local cond, ob, loc)
    | Expr (e, loc) ->
      let _ = get_type_expr cl fix_ty globl local e in
      Expr (opt_expr local e, loc)
    | Asminstr (e, loc) -> Asminstr (e, loc)
    | Del (e, destr, loc) ->
      let ty = get_type_expr cl fix_ty globl local e in
      begin
        match ty with
          | Tid s ->
            begin
              let rec find_destr cl =
                try
                  let m = Hashtbl.find cl.Class.m "destr" in
                  m.Class.used <- true;
                  destr := Some cl.Class.id
                with Not_found ->
                  begin
                    match cl.Class.mother with
                    | None -> destr := None
                    | Some mother -> find_destr mother
                  end
              in
              let cl =
                try
                  Hashtbl.find globl.Env.cl s
                with Not_found ->
                  raise (Cannot_find_class (s, loc))
              in
              find_destr cl
            end
          | Tnoconstr sign ->
            begin
              let _ = get_type_expr cl fix_ty globl local
                (Mcall (e, "destr", [], ref "", loc)) in
              Queue.add (ref destr) sign.destr
            end
          | _ -> raise (Not_an_object loc)
      end;
      Del (opt_expr local e, destr, loc)
  in
  List.map check body

and add_function ?c:(c=false) ?cl:(cl=None) ?idcl:(idcl=None)
    idmth globl al il loc =
  let rec set_arg = function
    | [] -> []
    | (c, h)::t ->
	(h, list2type c)::set_arg t
  in
  let rec set_arg_type env = function
    | [] -> []
    | (h, _)::t ->
      let ty =
        match Hashtbl.find env h with
        | Tnoconstr sign ->
          Class.constrain idmth h loc globl.Env.cl sign
        | x -> x
      in
      (h, ty)::set_arg_type env t
  in
  let m = Class.create_m () in
  m.Class.arg <- set_arg al;
  m.Class.c <- c;
  begin
    match cl with
    | None ->
      Hashtbl.replace globl.Env.func idmth m
    | Some cl ->
      Hashtbl.add cl.Class.m idmth m
  end;
  let env = Env.create_local () in
  List.iter
    (fun (id, ty) ->
      Hashtbl.add env id ty) m.Class.arg;
  let oil = check_bloc
    ~cl:(idcl) ~mth:(Some m) ~fix_ty:true globl env il
  in
  m.Class.arg <- set_arg_type env m.Class.arg;
  m, oil, env

and build_class idcl globl cl def =
  let rec build = function
    | Vdef (ty, id, loc) ->
      let t = list2type ty in
      begin
        match t with
        | Tid cl ->
	  begin
	    try
	      let _ = Hashtbl.find globl.Env.cl cl in ()
	    with Not_found -> raise (Cannot_find_class (cl, loc))
	  end
        | _ -> ()
      end;
      Hashtbl.add cl.Class.prop id t;
      Error.print_msg
        (Printf.sprintf "\t\tattr %s: %s" id (type2str t));
      Vdef (ty, id, loc)
    | Fdef ((id, al, il), loc) ->
      let m, oil, env = add_function ~cl:(Some cl) ~idcl:(Some idcl)
        id globl al il loc in
      Hashtbl.add globl.Env.local (idcl ^ "_" ^ id) env;
      Error.print_msg
        (Printf.sprintf "\t\tmethod %s: %s"
           id (func2str (List.map (fun (_, x) -> x) m.Class.arg) m.Class.ret));
      List.iter (fun x -> Error.print_msg
        (Printf.sprintf "\t\t\tvar %s" x)) (env2strl env);
      Error.print_msg "\t\tend";
      Fdef ((id, al, oil), loc)
  in
    List.map build def

and load_class loc globl id mother classdef =
  let mother =
    match mother with
    | None -> None
    | Some id ->
      try
        Some (Hashtbl.find globl.Env.cl id)
      with Not_found -> raise (Cannot_find_class (id, loc))
  in
  let cl = Class.create id mother in
  Hashtbl.add globl.Env.cl id cl;
  build_class id globl cl classdef

and check_body globl = function
  | Using (s, loc) ->
    if not
      (List.exists
         (fun x -> x = s)
         globl.Env.st_cl)
    then
      begin
        eval_file loc globl s;
        globl.Env.st_cl <- s::globl.Env.st_cl
      end;
    Using (s, loc)
  | Setup (b, loc) ->
    Error.print_msg "\tsetup";
    let s = check_bloc globl main_env b in
    List.iter (fun x -> Error.print_msg
      (Printf.sprintf "\t\tvar %s" x)) (env2strl main_env);
    Error.print_msg "\tend";
    Setup(s, loc)
  | Loop (b, loc) ->
    Error.print_msg "\tloop";
    let l = check_bloc globl main_env b in
    List.iter (fun x -> Error.print_msg
      (Printf.sprintf "\t\tvar %s" x)) (env2strl main_env);
    Error.print_msg "\tend";
    Loop(l, loc)
  | Func ((id, al, b), loc) ->
    let m, oil, env = add_function id globl al b loc in
    Hashtbl.add globl.Env.local id env;
    Error.print_msg
      (Printf.sprintf "\tfunc %s: %s"
         id (func2str (List.map (fun (_, x) -> x) m.Class.arg) m.Class.ret));
    List.iter (fun x -> Error.print_msg
      (Printf.sprintf "\t\tvar %s" x)) (env2strl env);
    Error.print_msg "\tend";
    Func ((id, al, oil), loc)
  | Extern (id, ty, loc) ->
    let ret = ref "undef" in
    let rec build_arg = function
      | [] -> []
      | h::[] -> ret := h; []
      | "void"::t -> build_arg t
      | h::t -> ([h], h)::build_arg t
    in
    let arg = build_arg ty in
    let m, _, _ = add_function ~c:true id globl arg
      [Return (Cast ([!ret],
                     ENone(loc),
                     loc),
               loc)] loc
    in
    Error.print_msg
      (Printf.sprintf "\textern %s: %s"
         id (func2str (List.map (fun (_, x) -> x) m.Class.arg) m.Class.ret));
    Extern (id, ty, loc)
  | ISR (id, b, loc) ->
    Error.print_msg
      (Printf.sprintf "\tISR %s" id);
    List.iter (fun x -> Error.print_msg
      (Printf.sprintf "\t\tvar %s" x)) (env2strl main_env);
    Error.print_msg "\tend";
    ISR(id, check_bloc globl main_env b, loc)
  | Class (id, mother, cd, loc) ->
    Error.print_msg
      (Printf.sprintf "\tclass %s" id);
    let ncd = load_class loc globl id mother cd in
    Error.print_msg "\tend";
    Class (id, mother, ncd, loc)
  | FSM (id, fd, loc) ->
    Class (id, None, load_class loc globl id None (Fsm.fsm2class fd), loc)

and eval_file loc globl id =
  Error.print_msg
    (Printf.sprintf "Including %s" id);
  let old_file = conf.cur_file in
  let cin = File.load (id ^ ".mr") in
  conf.cur_file <- (id ^ ".mr");
  let lexbuf = Lexing.from_channel cin in
  let body = (Parser.main Lexer.token lexbuf) in
  Queue.add (id, List.map (check_body globl) body)
    globl.Env.ast;
  conf.cur_file <- old_file
