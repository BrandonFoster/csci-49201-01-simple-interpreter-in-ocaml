(* OCaml Common-Lisp Interpreter
 * Programmer: Brandon Foster
 * Version: 0.0.1 *)

open Lexer
open Lexing
open Printf

let interactive =
  ()

let rec non_interactive ic =
  let lexbuf = Lexing.from_channel ic in
  let result = Parser.program Lexer.read lexbuf in
  match result with
  | Some lisp_obj ->
      CommonLisp.print_lisp_object lisp_obj;
      print_endline ""
  | None -> ()
  

let () =
  if (Array.length Sys.argv) > 1 then
    let ic = open_in Sys.argv.(1) in
    non_interactive ic
  else
    interactive