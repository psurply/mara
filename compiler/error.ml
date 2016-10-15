(*
** error.ml for Mara
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
** Started on  Wed Dec 26 11:47:42 2012 Pierre Surply
**  Last update Tue Aug 20 15:08:39 2013 Pierre Surply
*)

open Conf

let print_success size success =
  if conf.v >= 2 then
    begin
      if size < 12 then
        Printf.printf "\t";
      if size < 20 then
        Printf.printf "\t";
      if success then
        Printf.printf "\t\x1B[32msuccess\x1B[0m"
      else
        Printf.printf "\t\x1B[31mfailure\x1B[0m";
      if conf.v >= 3 then
        Printf.printf "\n%!"
      else
        Printf.printf "\r%!"
    end

let print_sample file line (b, e) =
  let cin = File.load file in
  for i = 0 to line - 2 do
    let _ = input_line cin in ()
  done;
  let l = input_line cin in
  let ul = Bytes.create
    (if e > 0 then e else b)
  in
  Printf.fprintf stderr
    "\x1B[34m%s\x1B[0m\n" l;
  if e = 0 then
    begin
      for i = 0 to b - 2 do
        Bytes.set ul i ' '
      done;
        Bytes.set ul (b - 1) '~'
    end
  else
    begin
      for i = 0 to b - 1 do
        Bytes.set ul i ' '
      done;
      for i = b to e - 1 do
        Bytes.set ul i '~'
      done
    end;
  Printf.fprintf stderr
    "\x1B[33m%s\x1B[0m\n" ul

let print_error (sp, ep) msg =
  let line = sp.Lexing.pos_lnum in
  let b = sp.Lexing.pos_cnum - sp.Lexing.pos_bol in
  let e = ep.Lexing.pos_cnum - ep.Lexing.pos_bol in
  print_success 9 false;
  if conf.v >= 1 then
    begin
      Printf.fprintf stderr
        "\x1B[34m**\x1B[0m In file \x1B[34m%s\x1B[0m, line \x1B[34m%d\x1B[0m, characters \x1B[34m%d\x1B[0m-\x1B[34m%d\x1B[0m
\x1B[34m**\x1B[0m %s"
        conf.cur_file line b e msg;
      print_sample conf.cur_file line (b, e);
    end;
  exit 2

let print_simple_error msg =
  if conf.v >= 1 then
    Printf.fprintf stderr
      "\x1B[31m**\x1B[0m %s" msg;
  exit 2

let print_msg msg =
  if conf.v >= 5 then
    Printf.printf "%s\n" msg
