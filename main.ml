(* OCaml Common-Lisp Interpreter
 * Programmer: Brandon Foster *)

open Lexer
open Lexing
open Printf

let non_interactive ic =
  ic |> Lexing.from_channel |> Parser.program Lexer.read |> Eval.run_program

let () =
  if (Array.length Sys.argv) > 1 then
    open_in Sys.argv.(1) |> non_interactive
  else
    ()