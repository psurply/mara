(*
** opt.ml for Mara
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
** Started on  Thu Jan  3 11:35:06 2013 Pierre Surply
**  Last update Sat Aug 24 15:02:02 2013 Pierre Surply
*)

open Ast
open Sem
open Typing
open Class

let optimize globl_env body =
  begin
    try
      Hashtbl.add globl_env.Env.local "_main" Sem.main_env;
      Queue.add ("", List.map (check_body globl_env) body)
	globl_env.Env.ast
    with
      | Cannot_find_class (cl, loc) ->
	  Error.print_error loc
	    (Printf.sprintf
                "Cannot find class %s\n"
                cl)
       | Undeclared_var (v, loc) ->
           Error.print_error loc
             (Printf.sprintf
                "Unbound variable %s\n"
                v)
       | Unknown_field (f, loc) ->
           Error.print_error loc
             (Printf.sprintf
                "This expression has not \'%s\' field\n"
                f)
       | Unknown_method (m, loc) ->
	  Error.print_error loc
	    (Printf.sprintf
	       "This expression has not \'%s\' method\n"
	       m)
       | Unknown_function (m, loc) ->
	 Error.print_error loc
	   (Printf.sprintf
	      "\'%s\' function is not defined\n"
	      m)
       | Not_an_object (loc) ->
	 Error.print_error loc "This expression is not an object\n"
       | Not_a_function (id, loc) ->
	 Error.print_error loc
           (Printf.sprintf "\'%s\' is not a function, it cannot be applied\n"
              id)
       | Given_arg (m, g, e, loc) ->
	 Error.print_error loc
	   (Printf.sprintf
	      "\'%s\' expected %d argument(s) (%d given)\n"
	      m e g)
       | No_init (c, loc) ->
	 Error.print_error loc
	   (Printf.sprintf "\'%s\' has not constructor\n" c)
       | Ret_nometh loc ->
	 Error.print_error loc
	   (Printf.sprintf
	      "Return statement must be used in a function/method\n")
       | Bad_type (e, g, loc) ->
	 Error.print_error loc
	   (Printf.sprintf
	      "This expression has type \'%s\'\nbut an expression was expected of type \'%s\'\n"
	      (type2str g) (type2str e))
       | Cannot_Match (ty, id_var, id_func, loc) ->
         Error.print_error loc
           (Printf.sprintf
              "In function \'%s\', cannot constrain \'%s\' which signature is :\n%s\n\n"
              id_func id_var ty)
       | Ambiguous_typing (ty, classes, id_var, id_func, loc) ->
         Error.print_error loc
           (Printf.sprintf
              "In function \'%s\', \'%s\' can be constrained to several classes :\n\t%s\nSignature :\n%s\n\n"
              id_func id_var classes ty)
  end
