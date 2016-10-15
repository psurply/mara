(*
**  fsm_errors.ml for Mara
**
**  Pierre Surply
**    pierre.surply@gmail.com
**    www.psurply.com
**
**  Started on Fri Mar  1 16:36:10 2013
*)

exception Trans_unknown_dest of string * (Lexing.position * Lexing.position)
exception Trans_unknown_input of string * (Lexing.position * Lexing.position)
