(*
** mmcu.ml for Mara
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
** Started on  Sat Dec 29 17:27:25 2012 Pierre Surply
** Last update Sun Jan 20 12:09:52 2013 Pierre Surply
*)

let mcu = 
"  atmega103
   atmega8u2
   atmega16u2
   atmega32u2
   atmega8
   atmega48
   atmega48a
   atmega48p
   atmega88
   atmega88a
   atmega88p
   atmega88pa
   atmega8515
   atmega8535
   atmega8hva
   atmega4hvd
   atmega8hvd
   atmega16
   atmega16a
   atmega161
   atmega162
   atmega163
   atmega164a
   atmega164p
   atmega165
   atmega165a
   atmega165p
   atmega168
   atmega168a
   atmega168p
   atmega169
   atmega169a
   atmega169p
   atmega169pa
   atmega16c1
   atmega16hva
   atmega16hva2
   atmega16hvb
   atmega16m1
   atmega16u4
   atmega32
   atmega323
   atmega324a
   atmega324p
   atmega324pa
   atmega325
   atmega325p
   atmega3250
   atmega3250p
   atmega328
   atmega328p
   atmega329
   atmega329p
   atmega329pa
   atmega3290
   atmega3290p
   atmega32c1
   atmega32hvb
   atmega32m1
   atmega32u4
   atmega32u6
   atmega406
   atmega64
   atmega640
   atmega644
   atmega644a
   atmega644p
   atmega644pa
   atmega645
   atmega645a
   atmega645p
   atmega6450
   atmega6450a
   atmega6450p
   atmega649
   atmega649a
   atmega649p
   atmega6490
   atmega6490a
   atmega6490p
   atmega64c1
   atmega64m1
   atmega64hve
   atmega128
   atmega1280
   atmega1281
   atmega1284p
   atmega128rfa1
   atmega2560
   atmega2561" 

let check_mmcu mmcu =
  if mmcu = "" then
    begin
      Printf.fprintf stderr
	"** Device type not specified (-mmcu)\n";
      exit 1
    end
  else
    let regex = Str.regexp (" *" ^ mmcu ^ "\n") in
      try
	let _ = Str.search_forward regex mcu 0 in ()
      with
	  Not_found ->
	    begin
	      Printf.fprintf stderr
		"** Unknown MCU \'%s\'\nKnown MCU :\n%s\n"
		mmcu mcu;
	      exit 1
	    end
