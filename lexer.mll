(* Common-Lisp Lexical Analyzer
 *)
{
  open Lexing
  open Parser

  (* Lisp Error Types *)
  exception ReaderError of string
  exception SyntaxError of string
  exception EOFError of string

  (* Lisp Lexing Types *)
  type constituent_trait =
    | Invalid
    | Alphabetic2
    | Alphadigit
    | PackageMarker
    | Plus
    | Minus
    | Dot
    | DecimalPoint
    | RatioMarker
    | DoubleFloat
    | Float
    | SingleFloat
    | LongFloat
    | ShortFloat
    | ExponentMarker

  type lisp_char =
    | Constituent of (char * char list)
  
}

(* Lisp Standard Character Repertoire *)
let del_char = ['\127']
let backspace_char = ['\b']
let newline_char = ['\n' '\r' '\012']
let whitespace_char = [' ' '\t']
let latin_char = ['a' - 'z' 'A' - 'Z']
let numeric_char = ['0' - '9']
let special_char = [
  '!'  '$'  '"'  '\'' '('  ')'  ','  '_'
  '-'  '.'  '/'  ':'  ';'  '?'  '+'  '<'
  '='  '>'  '#'  '%'  '&'  '*'  '@'  '['
  '\\' ']'  '{'  '|'  '}'  '`'  '^'  '~']

let lisp_std_char = del_char | backspace_char | newline_char | whitespace_char | latin_char | numeric_char | special_char

(* Lisp Syntax Types *)
let constituent_star = ['!' '?' '[' ']' '{' '}']
let constituent = ['$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '@' '^' '_' '~']
  | backspace_char | del_char | latin_char | numeric_char

let whitespace2 = whitespace_char | newline_char 
let term_macro = ['"' '\'' '(' ')' ',' ';' '`']
let non_term_macro = ['#']
let single_escape = ['\\']
let multi_escape = ['|']

(* Lisp Constituent Traits *)
let invalid_star = newline_char | whitespace_char
let invalid = backspace_char | del_char
let alphabetic2_star = ['"' '#' '\'' '(' ')' ',' ';' '\\' '`' '|']
let alphabetic2 = ['!' '$' '%' '&' '*' '<' '=' '>' '?' '@' '[' ']' '^' '_' '~' '{' '}' '+' '-' '.' '/']
let package_marker = [':']
let alphadigit = numeric_char | latin_char
let plus_sign = '+'
let minus_sign = '-'
let dot = '.'
let decimal_point = '.'
let ratio_marker = '/'
let double_float = ['d' 'D']
let ffloat = ['e' 'E']
let single_float = ['f' 'F']
let long_float = ['l' 'L']
let short_float = ['s' 'S']
let exponent_marker = ['d' 'D' 'e' 'E' 'f' 'F' 'l' 'L' 's' 'S']

(* Other *)
let uppercase = ['A' - 'Z']
let lowercase = ['a' - 'z']

(* Lisp Reader Algorithm *)

(* Step 1 *)
rule read =
  parse
  | eof { EOF }
    
    (* Step 2 *)
  | invalid as c { INVALID c }

    (* Step 3 *)
  | whitespace2 { WHITESPACE }

    (* Step 4 *)
  | term_macro as c { TERM_MACRO c  }
  | non_term_macro as c { NON_TERM_MACRO c }

    (* Step 5 *)
  | single_escape (constituent_star | constituent | alphabetic2_star) as str { CONSTITUENT ((String.get str 1), CommonLisp.alphabetic2::[]) }
  | single_escape eof { raise (EOFError ("end of file")) }

    (* Step 6 *)
  | multi_escape { MULTI_ESCAPE }

    (* Step 7 *)
  | '+' as c { CONSTITUENT (c, CommonLisp.alphabetic2::CommonLisp.plus_sign::[]) }
  | '-' as c { CONSTITUENT (c, CommonLisp.alphabetic2::CommonLisp.minus_sign::[]) }
  | '.' as c { CONSTITUENT (c, CommonLisp.alphabetic2::CommonLisp.dot::CommonLisp.decimal_point::[]) }
  | '/' as c { CONSTITUENT (c, CommonLisp.alphabetic2::CommonLisp.ratio_marker::[]) }
  | alphabetic2 as c { CONSTITUENT (c, CommonLisp.alphabetic2::[]) }

  | double_float as c { CONSTITUENT (c, CommonLisp.alphadigit::CommonLisp.double_float::CommonLisp.exponent_marker::[]) }
  | ffloat as c { CONSTITUENT (c, CommonLisp.alphadigit::CommonLisp.ffloat::CommonLisp.exponent_marker::[]) }
  | single_float as c { CONSTITUENT (c, CommonLisp.alphadigit::CommonLisp.single_float::CommonLisp.exponent_marker::[]) }
  | long_float as c { CONSTITUENT (c, CommonLisp.alphadigit::CommonLisp.long_float::CommonLisp.exponent_marker::[]) }
  | short_float as c { CONSTITUENT (c, CommonLisp.alphadigit::CommonLisp.short_float::CommonLisp.exponent_marker::[]) }
  | alphadigit as c { CONSTITUENT (c, CommonLisp.alphadigit::[]) }

  | package_marker as c { CONSTITUENT (c, CommonLisp.package_marker::[]) }

  
