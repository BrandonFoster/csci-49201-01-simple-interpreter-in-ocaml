(* Common-Lisp Module *)

open Printf

let invalid = 0
let alphabetic2 = 1
let alphadigit = 2
let package_marker = 3
let plus_sign = 4
let minus_sign = 5
let dot = 6
let decimal_point = 7
let ratio_marker = 8
let double_float = 9
let ffloat = 10
let single_float = 11
let long_float = 12
let short_float = 13
let exponent_marker = 14

type lisp_object =
  | Nil

(* printing *)
let print_lisp_object lisp_obj =
  ()