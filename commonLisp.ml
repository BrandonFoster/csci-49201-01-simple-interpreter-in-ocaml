(* Common-Lisp Module *)

open Printf

exception ExprError of string

type number = 
  | Integer of int
  | Ratio of (int * int)

type symbol = string

type atom =
  | Nil
  | Symbol of symbol
  | Number of number
  | String of string
  | Char of char

type expr =
  | Leaf of atom
  | Node of (expr * expr)

let car = function
  | Node (e1, _) -> e1
  | _ -> raise (ExprError "CAR-error")
let cdr = function
  | Node (_, e2) -> e2
  | _ -> raise (ExprError "CDR-error")
let cons e1 e2 =
  Node (e1, e2)

let rec cons_length = function
  | Leaf Nil -> 0
  | Leaf _ -> 1
  | Node (_, e2) -> 1 + cons_length e2