%{
(*
** parser.mly for Mara
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
** Started on  Fri Nov  9 15:58:54 2012 Pierre Surply
**  Last update Wed Aug 21 14:12:19 2013 Pierre Surply
*)

  open Parsing
  open Lexing

  let print_error pos =
    Error.print_error pos
      (Printf.sprintf "Syntax error\n")
%}

%token <int> INT
%token <char> CHAR
%token <string> STR
%token <string> ID
%token <string> CONST
%token <Ast.addr> ADDR
%token <string> ASMINSTR
%token NONE
%token ADD MINUS TIMES DIV MOD LSB RSB LOR LAND LXOR AND OR EQ NOT TILDE
%token DADD DMINUS DTIMES DDIV DMOD DLSB DRSB DLOR DLAND DLXOR
%token GT GE LT LE NEQ DEQ
%token COMMA
%token SETUP LOOP INTERRUPT CLASS FSM END ASM
%token RETURN VAR FUN METHOD NEW DEL EXTERN ATTR
%token STATE INPUT ACTION TRANS
%token IF THEN ELSE ELIF ENDIF
%token WHILE DO FOR TO DOWNTO DONE WAITFOR
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token DOT COLON QUEST DCOLON SCOLON
%token USING LAMBDA
%token EOL EOF
%left QUEST COLON
%left NOT TILDE
%left OR
%left AND
%left GT GE LT LE NEQ DEQ
%left LAND
%left LOR
%left LXOR
%left LSB RSB
%left ADD MINUS
%left TIMES DIV
%left MOD
%left LBRACKET
%nonassoc UMINUS
%left DOT
%left IF
%left DCOLON
%start main
%type <(Lexing.position * Lexing.position) Ast.t> main
%%

main:
  | b=llist(global) EOF { (b) }
  | error
      { print_error ($startpos, $endpos) }
;

global:
  | USING id=ID
        { Ast.Using (id, ($startpos, $endpos)) }
  | SETUP EOL b=body END
   { Ast.Setup(b, ($startpos, $endpos)) }
  | LOOP EOL b=body END
   { Ast.Loop(b, ($startpos, $endpos)) }
  | FUN id=ID LPAREN vl=arglist RPAREN EOL b=body END
   { Ast.Func((id, vl, b), ($startpos, $endpos))}
  | EXTERN id=ID COLON ty=cast
   { Ast.Extern(id, ty, ($startpos, $endpos))}
  | INTERRUPT id=CONST EOL b=body END
   { Ast.ISR(id, b, ($startpos, $endpos)) }
  | CLASS id=ID cd=classbody END
   { Ast.Class(id, None, cd, ($startpos, $endpos)) }
  | CLASS id=ID COLON mother=ID EOL cd=classbody END
   { Ast.Class(id, Some mother, cd, ($startpos, $endpos)) }
  | FSM id=ID EOL fd=fsmbody END
   { Ast.FSM(id, fd, ($startpos, $endpos)) }
;

llist(X):
  | EOL* x=X endl l=llist(X)    { x::l }
  | EOL*                        { [] }
;

%inline endl:
  | EOL {}
  | EOF {}
;

%inline classbody:
  | cl=llist(classdef)  { cl }
;

classdef:
  | METHOD id=ID LPAREN vl=arglist RPAREN EOL b=body END
        { Ast.Fdef((id, vl, b), ($startpos, $endpos))}
  | ATTR id=ID COLON ty=cast
        { Ast.Vdef(ty, id, ($startpos, $endpos)) }
  | error
        { print_error ($startpos, $endpos) }
;

%inline body:
  | b=llist(instr)      { b }
;

instr:
  | le=lexpr EQ e=expr
        { Ast.Affect(le, e, ($startpos, $endpos)) }
  | le=lexpr op=compound e=expr
        { Ast.Affect(le,
	             Ast.BinOp(op, le,
			       e,
			       ($startpos, $endpos)),
	             ($startpos, $endpos)) }
  | VAR id=ID EQ e=expr
        { Ast.Let(id, e, ($startpos, $endpos)) }
  | RETURN e=expr
	{ Ast.Return (e, ($startpos, $endpos)) }
  | IF c=expr THEN b=body ei=elif
	{ Ast.IfElse (c, b, ei, ($startpos, $endpos)) }
  | IF c=expr THEN b=body ENDIF
	{ Ast.If (c, b, ($startpos, $endpos)) }
  | i=instr IF c=expr
	{ Ast.If (c, [i], ($startpos, $endpos)) }
  | WHILE c=expr DO b=body DONE
	{ Ast.While (c, b, ($startpos, $endpos)) }
  | FOR id=ID EQ i=expr TO c=expr DO b=body DONE
        { Ast.For (true, id, i, c, b, ($startpos, $endpos)) }
  | FOR id=ID EQ i=expr DOWNTO c=expr DO b=body DONE
        { Ast.For (false, id, i, c, b, ($startpos, $endpos)) }
  | c=expr DCOLON i=instr
        { Ast.For
	  (true,
	   "__i",
	   Ast.Int(1, ($startpos, $endpos)),
	   c,
	   [i],
	   ($startpos, $endpos)) }
  | WAITFOR c=expr
        { Ast.While (c, [], ($startpos, $endpos)) }
  | DEL e=expr
        { Ast.Del (e, ref None, ($startpos, $endpos)) }
  | e=expr
	{ Ast.Expr(e, ($startpos, $endpos)) }
  | a=ASMINSTR
        { Ast.Asminstr(a, ($startpos, $endpos))}
  | error
	{ print_error ($startpos, $endpos) }
;

%inline compound:
  | DADD        { "+"  }
  | DMINUS      { "-"  }
  | DTIMES      { "*"  }
  | DDIV        { "/"  }
  | DMOD        { "%"  }
  | DLSB        { "<<" }
  | DRSB        { ">>" }
  | DLXOR       { "^"  }
  | DLOR        { "|"  }
  | DLAND       { "&"  }
;

elif:
  | ELSE b=body ENDIF   { b }
  | ELIF c=expr THEN b=body ei=elif
        { [Ast.IfElse (c, b, ei, ($startpos, $endpos))] }
  | ELIF c=expr THEN b=body ENDIF
        { [Ast.If (c, b, ($startpos, $endpos))] }
;

lexpr:
  | e=expr DOT i=ID
        { Ast.Field(e, i, ref "", ($startpos, $endpos)) }
  | i=ID
        { Ast.Var(i, ($startpos, $endpos)) }
  | addr=ADDR
        { Ast.Addr(addr, ($startpos, $endpos)) }
  | o=expr LBRACKET e=expr RBRACKET
        { Ast.Get(o, e, ref 0, ref None, ($startpos, $endpos)) }
;

%inline arg:
  | c=cast COLON i=ID   { (c, i) }
  | i=ID                { (["undef"], i) }
;

%inline arglist:
  | vl=separated_list(COMMA, arg)       { vl }
;

%inline exprlist:
  | el=separated_list(COMMA, expr)      { el }
;

cast:
  | c=separated_nonempty_list(TRANS, ID)      { c }
;

expr:
  | NONE
        { Ast.ENone(($startpos, $endpos)) }
  | i=INT
        { Ast.Int(i, ($startpos, $endpos)) }
  | c=CHAR
        { Ast.Int(Char.code c, ($startpos, $endpos)) }
  | str=STR+
        { Ast.Str(String.concat "" str, ($startpos, $endpos)) }
  | LBRACE v=separated_list(SCOLON, expr) RBRACE
        { Ast.Vect(v, ($startpos, $endpos)) }
  | LBRACKET l=separated_list(SCOLON, expr) RBRACKET
        { Ast.New("list", [Ast.Vect(l, ($startpos, $endpos))],
                  ($startpos, $endpos)) }
  | addr=ADDR
        { Ast.Addr(addr, ($startpos, $endpos)) }
  | c=CONST+
        { Ast.Const(c, ($startpos, $endpos)) }
  | LPAREN c=cast COLON e=expr RPAREN
        { Ast.Cast(c, e, ($startpos, $endpos)) }
  | id=ID
        { Ast.Var(id, ($startpos, $endpos)) }
  | LPAREN e=expr RPAREN
        { e }
  | le=expr op=binop re=expr
	{ Ast.BinOp(op, le, re, ($startpos, $endpos)) }
  | MINUS e=expr %prec UMINUS
        { Ast.UniOp("-", e, ($startpos, $endpos)) }
  | op=uniop e=expr
        { Ast.UniOp(op, e, ($startpos, $endpos)) }
  | c=expr QUEST t=expr COLON f=expr
        { Ast.Tern(c, t, f, ($startpos, $endpos)) }
  | NEW cl=ID LPAREN arg=exprlist RPAREN
	{ Ast.New(cl, arg, ($startpos, $endpos)) }
  | ASM code=STR
	{ Ast.Asm(None, code, ($startpos, $endpos)) }
  | ASM LBRACE e=expr RBRACE code=STR
        { Ast.Asm(Some e, code, ($startpos, $endpos)) }
  | o=expr DOT m=ID LPAREN arg=exprlist RPAREN
        { Ast.Mcall(o, m, arg, ref "", ($startpos, $endpos)) }
  | f=ID LPAREN arg=exprlist RPAREN
        { Ast.Fcall(f, arg, ref false, ($startpos, $endpos)) }
  | o=expr DOT f=ID
        { Ast.Field(o, f, ref "", ($startpos, $endpos)) }
  | o=expr LBRACKET e=expr RBRACKET
        { Ast.Get(o, e, ref 0, ref None, ($startpos, $endpos)) }
  | LPAREN LAMBDA LPAREN vl=arglist RPAREN COLON
     b=separated_list(SCOLON, instr) RPAREN
        { Ast.Lambda(vl, ref b, ref "", ($startpos, $endpos)) }
;

%inline binop:
  | ADD         { "+"   }
  | MINUS       { "-"   }
  | TIMES       { "*"   }
  | DIV         { "/"   }
  | MOD         { "%"   }
  | LSB         { "<<"  }
  | RSB         { ">>"  }
  | LOR         { "|"   }
  | LAND        { "&"   }
  | LXOR        { "^"   }
  | AND         { "and" }
  | OR          { "or"  }
  | GT          { ">"   }
  | GE          { ">="  }
  | LT          { "<"   }
  | LE          { "<="  }
  | NEQ         { "!="  }
  | DEQ         { "=="  }
;

%inline uniop:
  | NOT         { "not" }
  | TILDE       { "~"   }
;

%inline fsmbody:
  | f=llist(fsmdef)     { f }
;

fsmdef:
  | ty=ID id=ID                         { Ast.AVdef(ty, id) }
  | STATE id=ID EOL s=statebody END     { Ast.AState(id, s) }
  | INPUT id=ID EQ c=expr               { Ast.AInput(id, c) }
;

%inline statebody:
  | s=llist(statedef)   { s }
;

statedef:
  | ACTION EOL b=body END       { Ast.SAction(b) }
  | id=ID TRANS dest=ID
        { Ast.STrans(Some id, dest, ($startpos, $endpos)) }
  | TRANS dest=ID
        { Ast.STrans(None, dest, ($startpos, $endpos)) }
;
