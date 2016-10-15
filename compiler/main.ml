(*
** main.ml for Mara
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
** Started on  Fri Nov  9 15:58:29 2012 Pierre Surply
**  Last update Tue Aug 20 17:46:42 2013 Pierre Surply
*)

open Conf

let print_version nl =
  Printf.printf
    "Mara compiler revision %d

Copyright (C) 2013 Pierre Surply
This program comes with ABSOLUTELY NO WARRANTY.
This is free software, and you are welcome to redistribute it
under certain conditions; see the file named COPYING for details.

email:\t\tpierre.surply@gmail.com
website:\tmara.psurply.com

Standard library directory: %s\n"
    conf.version stdlib_path;
  if nl then
    Printf.printf "\n"

let usage = Printf.sprintf "usage : %s <options> <files>\n" Sys.argv.(0)
let args = Arg.align [("-mmcu",
	               Arg.String (fun x -> conf.mmcu <- x),
	               "<mcu> Targeted microcontroller model (default: atmega8)");
	              ("-S",
	               Arg.Unit (fun () -> conf.compile_only <- true),
	               " Compile only; Do not assemble or link");
	              ("-c",
	               Arg.Unit (fun () -> conf.do_not_link <- true),
	               " Compile and assemble, but do not link");
	              ("-I",
	               Arg.String (fun x -> conf.inc <- x::conf.inc),
	               "<dir> Include folder");
	              ("-v",
	               Arg.Unit (fun _ -> print_version false; exit 0),
	               " Print compiler version and location of standard library and exit");
	              ("-verbose",
	               Arg.Int (fun i -> conf.v <- i),
	               Printf.sprintf
                         "<verbosity> Set verbosity level (default: %d)
        0: quiet
        1: only print error messages
        2: print processing step dynamically
        3: print processing step
        4: print calls to external program
        5: print infered types"
                         conf.v);
	              ("-ccopt",
	               Arg.String (fun x -> conf.cflags <- x::conf.cflags),
                       "<opt> Pass option <opt> to the C compiler and linker");
                      ("-cclib",
	               Arg.String (fun x -> conf.lflags <- x::conf.lflags),
                       "<opt> Pass option <opt> to the C linker");
	             ]

let exec ?show_status:(show_status=true) name arg =
  let print_cmd arg =
    List.iter
      (fun x -> Printf.printf "%s " x)
      (Array.to_list arg);
    Printf.printf "\n%!"
  in
  let name_size = String.length name in
  let read, write = Unix.pipe() in
  if (Unix.fork () == 0) then
    begin
      Unix.close read;
      Unix.dup2 write Unix.stderr;
      Unix.execvp arg.(0) arg
    end
  else
    begin
      Unix.close write;
      Unix.dup2 read Unix.stdin;
      if conf.v >= 4 then
        print_cmd arg;
      if conf.v >= 2 then
        Printf.printf "%s...%s\x1B[34m%!" name
          (if show_status then "" else "\n");
      let _, status = Unix.wait () in
      if show_status then
        begin
          match status with
          | Unix.WEXITED ret when ret = 0 ->
            Error.print_success name_size true;
            Unix.close read
          | _ ->
            Error.print_success name_size false;
            if conf.v >= 1 then
              begin
                if conf.v < 4 then
                  print_cmd arg;
                Printf.printf "\x1B[31m%!";
                let buff = Bytes.create 512 in
                let i = Unix.read Unix.stdin buff 0 512 in
                let _ = Unix.write Unix.stderr buff 0 i in
                Printf.printf "\x1B[0m%!";
              end;
            Unix.close read;
            exit 2
        end
      else
        Printf.printf "\x1B[0m%!"
    end

let arg () =
  Arg.parse args
    (let regex = Str.regexp "\\(.+\\)\\.\\(.+\\)" in
      (fun f ->
        if Str.string_match regex f 0 then
          match Str.matched_group 2 f with
          | "mr" ->
            let name = Str.matched_group 1 f in
            Queue.push name conf.files;
            conf.afiles <- (build_path ^ name)::conf.afiles;
            conf.ofiles <- (build_path ^ name)::conf.ofiles;
          | "c" ->
            let name = Str.matched_group 1 f in
            conf.cfiles <- name::conf.cfiles;
            conf.ofiles <- (build_path ^ name)::conf.ofiles;
          | "o" ->
            let name = Str.matched_group 1 f in
            conf.ofiles <- name::conf.ofiles;
          | "s" ->
            let name = Str.matched_group 1 f in
            conf.afiles <- name::conf.afiles;
            conf.ofiles <- (build_path ^ name)::conf.ofiles
          | ext ->
            Error.print_simple_error
              (Printf.sprintf
                 "Don't know what to do with .%s extension\n"
                 ext);
        else
          begin
            Queue.push f conf.files;
            conf.afiles <- (build_path ^ f)::conf.afiles;
            conf.ofiles <- (build_path ^ f)::conf.ofiles
          end))
    usage

let gen_asm ast dst =
  let globl_env = Env.create_globl () in
  Opt.optimize globl_env ast;
  Gen.gen globl_env dst

let build_hex f =
  let regex = Str.regexp "^.*/\\([^/]+\\)" in
  begin
    if not conf.compile_only then
      List.iter (fun x ->
        exec "Assembling"
          [| "avr-gcc";
             "-mmcu=" ^ conf.mmcu;
             "-x"; "assembler-with-cpp"; "-c";
             "-o"; Printf.sprintf "%s.o"
               (if conf.do_not_link then
                   let _ = Str.search_forward regex x 0 in
                   Str.matched_group 1 x
                else x);
             Printf.sprintf "%s.s" x
          |]) conf.afiles
    else
      let arg = [| "mv"; build_path ^ f ^ ".s"; f ^ ".s"|] in
      if (Unix.fork () == 0) then
        Unix.execvp arg.(0) arg;
      let _ = Unix.wait () in ();
  end;
  List.iter (fun c ->
    exec (Printf.sprintf "Compiling %s" c)
      (Array.of_list
         (List.concat [["avr-gcc";
                        "-mmcu=" ^ conf.mmcu];
                       conf.cflags;
                       if conf.compile_only then ["-S"] else ["-c"];
                       List.concat (List.map (fun x -> ["-I"; x]) conf.inc);
                       ["-o"; Printf.sprintf "%s%s.%s"
                         (if conf.compile_only || conf.do_not_link then ""
                          else build_path) c
                         (if conf.compile_only then "s" else "o");
                        c ^ ".c"];
                      ])))
    conf.cfiles;
  if not conf.do_not_link && not conf.compile_only then
    begin
      exec "Linking"
        (Array.of_list
           (List.concat [["avr-gcc";
                          "-mmcu=" ^ conf.mmcu];
                         conf.cflags;
                         conf.lflags;
                         List.map (fun x ->
                           Printf.sprintf "%s.o" x)
                            conf.ofiles;
                          ["-o"; Printf.sprintf "/tmp/_mbuild/%s.elf" f]
                         ]));
       exec "Converting elf to ihex"
         [| "avr-objcopy";
            "-O"; "ihex";
            Printf.sprintf "/tmp/_mbuild/%s.elf" f;
            f ^ ".hex"
         |];
       if conf.v >= 3 then
         exec "Checking" ~show_status:false
           [| "avr-size"; "--format=SysV";
              Printf.sprintf "/tmp/_mbuild/%s.elf" f;
           |]
     end

let show_time () =
  let t = Unix.gmtime (Unix.times ()).Unix.tms_utime in
  Printf.printf
    "\x1B[32mFinished (%02d:%02d:%02d)\x1B[0m                            \n"
    t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec

let loop () =
  let f = Queue.take conf.files in
  let file = f ^ ".mr" in
  let dst = build_path ^ f ^ ".s" in
  conf.cur_file <- file;
  let cin = File.load file in
  let cout = open_out dst in
  let lexbuf = Lexing.from_channel cin in
  if conf.v >= 2 then
    Printf.printf "Compiling...%!";
  if conf.v >= 5 then
    Printf.printf "\n";
  let ast = (Parser.main Lexer.token lexbuf) in
  gen_asm ast cout;
  close_out cout;
  Error.print_success 9 true;
  build_hex f;
  if conf.v >= 2 then
  show_time ()

let main () =
  arg ();
  if conf.v >= 4 then
    print_version true;
  begin
    if Queue.is_empty conf.files then
      Arg.usage args usage
    else
      try
        Mmcu.check_mmcu conf.mmcu;
        let arg = [| "mkdir"; "-p"; build_path |] in
        if (Unix.fork () == 0) then
          Unix.execvp arg.(0) arg;
        let _ = Unix.wait () in ();
        while not (Queue.is_empty conf.files) do
          loop ()
        done
      with File.Cannot_find f ->
        Error.print_simple_error
          (Printf.sprintf "Cannot find \'%s\'\n" f)
  end;
  exit 0

let _ = main ()
