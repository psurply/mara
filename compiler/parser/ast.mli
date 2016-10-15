(*
** ast.ml for Mara
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
** Started on  Fri Nov  9 19:40:14 2012 Pierre Surply
**  Last update Wed Aug 21 14:10:33 2013 Pierre Surply
*)

type addr =
| AString       of string
| ADString      of string * string
| AInt          of int
| ADInt         of int * int

type ('instr, 'loc) expr =
| Int		of int * 'loc
| ENone         of 'loc
| Str		of string * 'loc
| Vect		of (('instr, 'loc) expr) list * 'loc
| Addr          of addr * 'loc
| Const         of string list * 'loc
| Var		of string * 'loc
| Field         of ('instr, 'loc) expr * string * string ref * 'loc
| Get		of ('instr, 'loc) expr * ('instr, 'loc) expr * int ref *
                   (('instr, 'loc) expr option) ref * 'loc
| BinOp         of string * ('instr, 'loc) expr * ('instr, 'loc) expr * 'loc
| UniOp         of string * ('instr, 'loc) expr * 'loc
| Tern          of ('instr, 'loc) expr * ('instr, 'loc) expr *
                   ('instr, 'loc) expr * 'loc
| Fcall         of string * (('instr, 'loc) expr) list * bool ref * 'loc
| Mcall         of ('instr, 'loc) expr * string * (('instr, 'loc) expr) list *
                   string ref * 'loc
| New		of string * (('instr, 'loc) expr) list * 'loc
| Asm		of ('instr, 'loc) expr option * string * 'loc
| Cast          of string list * ('instr, 'loc) expr * 'loc
| Lambda        of ((string list) * string) list * ('instr list) ref *
                   string ref * 'loc

type 'loc instr =
| Let		of string * ('loc instr, 'loc) expr * 'loc
| Affect	of ('loc instr, 'loc) expr * ('loc instr, 'loc) expr * 'loc
| Return	of ('loc instr, 'loc) expr * 'loc
| If		of ('loc instr, 'loc) expr * ('loc instr) list * 'loc
| IfElse	of
    ('loc instr, 'loc) expr * ('loc instr) list * ('loc instr) list * 'loc
| While         of ('loc instr, 'loc) expr * ('loc instr) list * 'loc
| For		of
    bool * string * ('loc instr, 'loc) expr *
      ('loc instr, 'loc) expr * ('loc instr) list * 'loc
| Del           of ('loc instr, 'loc) expr * (string option) ref * 'loc
| Expr          of ('loc instr, 'loc) expr * 'loc
| Asminstr      of string * 'loc

type 'loc bloc = ('loc instr) list

type 'loc func = string *
    ((string list) * string) list * ('loc instr) list

type 'loc classdef =
| Vdef		of string list * string * 'loc
| Fdef		of 'loc func * 'loc

type 'loc statedef =
| SAction	of ('loc instr) list
| STrans	of string option * string * 'loc

type 'loc fsmdef =
| AVdef		of string * string
| AState 	of string * ('loc statedef) list
| AInput	of string * ('loc instr, 'loc) expr

type 'loc body =
| Using         of string * 'loc
| Setup         of ('loc bloc) * 'loc
| Loop          of ('loc bloc) * 'loc
| Func          of 'loc func * 'loc
| Extern        of string * string list * 'loc
| ISR           of string * ('loc bloc) * 'loc
| Class         of string * string option * ('loc classdef) list * 'loc
| FSM	 	of string * ('loc fsmdef) list * 'loc

type 'loc t = ('loc body) list
