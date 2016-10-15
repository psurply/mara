(*
** lexer.mll for Mara
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
** Started on  Fri Nov  9 15:58:41 2012 Pierre Surply
**  Last update Tue Aug 20 21:14:42 2013 Pierre Surply
*)

{
  open Parser

  let print_lexer_error c =
    Error.print_simple_error
      (Printf.sprintf "Unknown token : %c\n" c)

  let nl lexbuf =
    let p = lexbuf.Lexing.lex_curr_p in
      lexbuf.Lexing.lex_curr_p <- {
	p with
	  Lexing.pos_lnum = p.Lexing.pos_lnum + 1;
	  Lexing.pos_bol  = p.Lexing.pos_cnum;
      }

  let nnl n lexbuf =
    for i = 1 to n do
      nl lexbuf
    done

  let strbuf = Buffer.create 128
  let asmon = ref false

  let ht_kwd = Hashtbl.create 13
  let kwd =
    [("setup",		SETUP);
     ("loop",		LOOP);
     ("class",		CLASS);
     ("fsm", 		FSM);
     ("end",		END);
     ("new",		NEW);
     ("del",		DEL);
     ("return",		RETURN);
     ("if",		IF);
     ("then",		THEN);
     ("endif",		ENDIF);
     ("else",		ELSE);
     ("elif",		ELIF);
     ("while",		WHILE);
     ("for",		FOR);
     ("waitfor",	WAITFOR);
     ("do",		DO);
     ("done",		DONE);
     ("to",		TO);
     ("downto",		DOWNTO);
     ("var",		VAR);
     ("func",		FUN);
     ("method",		METHOD);
     ("attr",		ATTR);
     ("interrupt",      INTERRUPT);
     ("lambda",		LAMBDA);
     ("none",		NONE);
     ("using",		USING);
     ("action",		ACTION);
     ("state",		STATE);
     ("input",		INPUT);
     ("extern",		EXTERN);
    ]
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add ht_kwd kwd tok) kwd

  let ht_sym = Hashtbl.create 13
  let sym =
    [("+",	ADD);
     ("++",	DADD);
     ("-",	MINUS);
     ("--",	DMINUS);
     ("*",	TIMES);
     ("**",	DTIMES);
     ("/",	DIV);
     ("//",	DDIV);
     ("%",	MOD);
     ("%%",	DMOD);
     ("(",	LPAREN);
     (")",	RPAREN);
     ("{",	LBRACE);
     ("}",	RBRACE);
     ("[",	LBRACKET);
     ("]",	RBRACKET);
     ("or",	OR);
     ("and",	AND);
     ("bitls",	LSB);
     ("<<",	LSB);
     ("<<<<",	DLSB);
     ("bitrs",	RSB);
     (">>",	RSB);
     (">>>>",	DRSB);
     ("bitor",	LOR);
     ("|",	LOR);
     ("||",	DLOR);
     ("bitand",	LAND);
     ("&",	LAND);
     ("&&",	DLAND);
     ("bitxor",	LXOR);
     ("^",	LXOR);
     ("^^",	DLXOR);
     ("<-",	EQ);
     (",",	COMMA);
     (">",	GT);
     (">=",	GE);
     ("<",	LT);
     ("<=",	LE);
     ("<>",	NEQ);
     ("!=",	NEQ);
     ("=",	DEQ);
     (".",	DOT);
     (":",	COLON);
     ("::",	DCOLON);
     (";",	SCOLON);
     ("?",	QUEST);
     ("not",	NOT);
     ("compl",	TILDE);
     ("~",	TILDE);
     ("->",	TRANS);
    ]
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add ht_sym kwd tok) sym

}

let white = [' ' '\t']

let ident = ['a'-'z'] ['a'-'z' '0'-'9' '_']*

let const = ['A'-'Z'] ['A'-'Z' '0'-'9' '_']*

let int0 = "false"

let int1 = "true"

let symba = "+" | "++" | "-" | "--" | "*" | "**" | "/" | "//" | "%" | "%%" |
  "(" | ")" | "{" | "}" | "<<" | "bitls" | "<<<<" | ">>" | "bitrs" | ">>>>" |
  "|" | "bitor" | "||" | "&" | "bitand" | "&&" | "bitxor" | "^" | "^^" | "or" |
  "and" | "<-" | "->" | "," | "<" | ">" | "<=" | ">=" | "<>" | "!=" | "=" |
  "[" | "]" | "." | ":" | "not" | "~" | "compl" | "_" | "?" | "::" | ";"

let integer = ['0'-'9']+ | ("0b" ['0' '1']+) | ("0x" ['0'-'9' 'A'-'F' 'a'-'f']+)

let asm_instr = "sei" | "cli" | "wdr" | "sleep" | "nop" | "break"

rule token = parse
    | white                     { token lexbuf }
    | "\\" [' ' '\t']* "\n"     { nl lexbuf; token lexbuf }
    | ['\n']+ as lxm		{ nnl (String.length lxm) lexbuf; EOL }
    | "#"			{ comment lexbuf }
    | asm_instr as lxm          { ASMINSTR(lxm) }
    | integer as lxm		{ INT(int_of_string lxm) }
    | int0			{ INT(0) }
    | int1			{ INT(1) }
    | '\''([^'\''] as c)'\''	{ CHAR(c) }
    | '@'(integer as addr)      { ADDR(Ast.AInt(int_of_string addr)) }
    | '@'(integer as haddr)':'(integer as laddr)
        { ADDR(Ast.ADInt(int_of_string haddr, int_of_string laddr)) }
    | '@'(const as addr)        { ADDR(Ast.AString(addr)) }
    | '@'(const as haddr)':'(const as laddr)
        { ADDR(Ast.ADString(haddr, laddr)) }
    | "asm"                     { asmon := true; ASM }
    | '"'
        {
          Buffer.reset strbuf;
          if !asmon then
            begin
              asmstr strbuf lexbuf;
              asmon := false;
            end
          else
            str strbuf lexbuf;
          STR (Buffer.contents strbuf)
        }
    | symba as s		{ Hashtbl.find ht_sym s }
    | const as c                { CONST(c) }
    | ident as i		{ try
				    Hashtbl.find ht_kwd i
				  with
				    Not_found -> ID(i)}
    | eof			{ EOF }
    | _ as c                    { print_lexer_error c }

and comment = parse
    | '\n'		{ nl lexbuf; EOL }
    | eof		{ EOF }
    | _			{ comment lexbuf }

and str s = parse
    | '"'               { () }
    | '\n'              { nl lexbuf;
                          Buffer.add_string s "\\n";
                          str s lexbuf}
    | _ as c            { Buffer.add_char s c;
                          str s lexbuf}
and asmstr s = parse
    | '"'               { () }
    | '\n' [' ' '\t']*  { nl lexbuf;
                          Buffer.add_string s "\n\t";
                          asmstr s lexbuf}
    | _ as c            { Buffer.add_char s c;
                          asmstr s lexbuf}
