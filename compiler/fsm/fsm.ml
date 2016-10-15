(*
**  fsm.ml for Mara
**
**  Pierre Surply
**    pierre.surply@gmail.com
**    www.psurply.com
**
**  Started on Fri Mar  1 16:36:10 2013
*)

open Ast
open Fsm_errors

type 'loc fsm =
{
  mutable def	: (string * string) list;
  inputs	: (string, ('loc instr, 'loc) expr) Hashtbl.t;
  states	:
    (string, 'loc State.state * (('loc statedef) list)) Hashtbl.t;
  states_id	: (string, int) Hashtbl.t;
  mutable size	: int
}

let create () =
{
  def = [];
  inputs = Hashtbl.create 13;
  states = Hashtbl.create 13;
  states_id = Hashtbl.create 13;
  size = 0
}

let ast2fsm fsmdef =
  let fsm = create () in
  let build = function
    | AVdef (ty, id)		-> fsm.def <- (ty, id)::fsm.def
    | AInput (id, e)		-> Hashtbl.add fsm.inputs id e
    | AState (id, statedef)	->
      Hashtbl.add fsm.states id
      (State.ast2state statedef, statedef);
      Hashtbl.add fsm.states_id id fsm.size;
      fsm.size <- fsm.size + 1
  in
  begin
    try
      List.iter build fsmdef;
      Hashtbl.iter (State.set_trans fsm.inputs fsm.states)
        fsm.states
    with
      | Trans_unknown_dest (dest, loc) ->
	Error.print_error loc
          (Printf.sprintf
	     "Cannot find state %s\n"
	     dest)
      | Trans_unknown_input (input, loc) ->
	Error.print_error loc
	  (Printf.sprintf
	     "Cannot find input %s\n"
	     input)
  end;
  try
    let _ = Hashtbl.find fsm.states "start" in
    fsm
  with Not_found ->
    Printf.fprintf stderr
      "** The FSM must have a \"start\" state\n";
      exit 3

let fsm2class fsmdef =
  let loc =
    let fake_pos =
      {
        Lexing.pos_fname = "FSM pos";
        Lexing.pos_lnum = -1;
 	Lexing.pos_bol = -1;
 	Lexing.pos_cnum = -1;
      }
    in
    (fake_pos, fake_pos)
  in
  let fsm = ast2fsm fsmdef in
  let classdef = ref [] in
  let build_trans_call id n elseb =
    [IfElse (BinOp ("==",
		   Field (Var ("self", loc), "state", ref "", loc),
		   Int (n, loc), loc),
	    [Affect (Var ("ret", loc),
	     Mcall(Var ("self", loc), id ^ "_trans", [], ref "", loc),
	     loc)],
	    elseb,
	    loc)]
  in
  let build_action_call id n elseb =
    [IfElse (BinOp ("==",
		   Field (Var ("self", loc), "state", ref "", loc),
		   Int (n, loc), loc),
	    [Expr (Mcall (Var ("self", loc),
			  id ^ "_action", [],
			  ref "", loc),
	           loc)],
	    elseb,
	    loc)]
  in
  classdef := (Fdef (("update",
		      [],
		      [Let ("ret", Int (-1, loc), loc);
		       IfElse (Field (Var ("self", loc),
				      "waiting", ref "", loc),
			       Hashtbl.fold build_trans_call fsm.states_id []@
			         [If (BinOp("!=",
					    Var ("ret", loc),
					    Int (-1, loc), loc),
				      [Affect (Field (Var ("self", loc),
				     	              "state", ref "", loc),
				               Var ("ret", loc), loc);
				       Affect (Field (Var ("self", loc),
				     	              "waiting", ref "", loc),
				               Int (0, loc), loc)], loc)],
			       (Hashtbl.fold build_action_call fsm.states_id [])@
			         [Affect (Field (Var ("self", loc),
				     	         "waiting", ref "", loc),
				          Int (1, loc), loc)],
			       loc);
		       Return (Field (Var ("self", loc),
				      "state", ref "", loc), loc)]), loc))::
    !classdef;
  let set_trans (cond, dst) elseb =
    [IfElse ((match cond with
    | None 		-> Int (1, loc)
    | Some input 	-> Hashtbl.find fsm.inputs input),
	     [Return (Int (Hashtbl.find fsm.states_id dst,
			   loc), loc)],
	     elseb,
	     loc)]
  in
  let set_state_func id (state, _) =
    classdef := (Fdef ((id ^ "_action",
		        [],
		        state.State.action), loc))::
      (Fdef ((id ^ "_trans",
	      [],
	      List.fold_right set_trans state.State.trans
	        [Return (Int (-1, loc), loc)]), loc))::
      (Fdef (("is_" ^ id,
	     [],
	     [Return (BinOp("==",
			    Field (Var ("self", loc),
				   "state", ref "", loc),
			    Int (Hashtbl.find fsm.states_id id, loc),
			    loc), loc)]), loc))::
      !classdef
  in
  Hashtbl.iter set_state_func fsm.states;
  classdef := (Fdef (("init",
		     [],
		      [Affect (Field (Var ("self", loc),
				     "state", ref "", loc),
			      Int (Hashtbl.find fsm.states_id "start", loc), loc);
		      Affect (Field (Var ("self", loc),
				     "waiting", ref "", loc),
			      Int (0, loc), loc);
		      Return (Var ("self", loc), loc)
		     ]), loc))::
    !classdef;
  List.iter
    (fun (ty, id) ->
      classdef := (Vdef ([ty], id, loc))::!classdef)
    fsm.def;
  classdef := (Vdef (["integer"], "state", loc))::
    (Vdef (["integer"], "waiting", loc))::
    !classdef;
  !classdef
