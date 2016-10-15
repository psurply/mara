(*
**  state.ml for Mara
**
**  Pierre Surply
**    pierre.surply@gmail.com
**    www.psurply.com
**
**  Started on Fri Mar  1 16:41:43 2013
*)

open Ast
open Fsm_errors

type 'loc state =
{
  mutable action	: ('loc instr) list;
  mutable trans		: (string option * string) list;
}

let create () =
{
  action = [];
  trans = [];
}

let ast2state statedef =
  let state = create () in
  let build = function
    | SAction il	-> state.action <- il
    | _ -> ()
  in
  List.iter build statedef;
  state

let set_trans inputs states id (state, statedef) =
  let build = function
    | STrans (org, dst, loc)	->
      begin
	match org with
	  | None	-> ()
	  | Some o	->
	    begin
	      try
	        let _ = Hashtbl.find inputs o in ()
	      with Not_found -> raise (Trans_unknown_input (o, loc))
	    end
      end;
      begin
	try
	  let _ = Hashtbl.find states dst in ()
	with Not_found -> raise (Trans_unknown_dest (dst, loc))
      end;
      state.trans <- (org, dst)::state.trans
    | _ -> ()
  in
  List.iter build statedef;
  state.trans <- List.rev state.trans
