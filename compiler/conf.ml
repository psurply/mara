(*
** conf.ml for Mara
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
** Started on  Tue Dec 25 17:11:59 2012 Pierre Surply
**  Last update Tue Aug 20 17:13:39 2013 Pierre Surply
*)

type t =
    {
      version                   : int;
      mutable mmcu              : string;
      mutable cur_file          : string;
      files                     : string Queue.t;
      mutable cfiles            : string list;
      mutable afiles            : string list;
      mutable ofiles            : string list;
      mutable cflags            : string list;
      mutable lflags            : string list;
      mutable inc               : string list;
      mutable v                 : int;
      mutable compile_only      : bool;
      mutable do_not_link       : bool;
    }

let stdlib_path = "/usr/local/avr/lib/mara/"
let build_path = "/tmp/_mbuild/"

let conf =
  {
    version = 13;
    mmcu = "atmega8";
    cur_file = "";
    files = Queue.create ();
    cfiles = [];
    afiles = [];
    ofiles = [];
    cflags = [];
    lflags = [];
    inc = ["."; stdlib_path];
    v = 2;
    compile_only = false;
    do_not_link = false;
  }
