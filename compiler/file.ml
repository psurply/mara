(*
** file.ml for Mara
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
** Started on  Tue Dec 25 18:12:26 2012 Pierre Surply
**  Last update Wed Jul 10 17:31:50 2013 Pierre Surply
*)

open Conf

exception Cannot_find of string
let load id =
  let rec rload = function
    | [] -> raise (Cannot_find id)
    | h::t ->
	try
	  open_in (h ^ "/" ^ id)
	with Sys_error _ -> rload t
  in
    rload conf.inc
