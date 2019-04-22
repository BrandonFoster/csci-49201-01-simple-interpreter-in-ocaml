(* Common-Lisp Lexical Analyzer *)
{
  open Parser

  exception EOFError of string
  exception ReaderError of string
  exception NotSupportedError of string
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

let invalid = del_char | backspace_char

let whitespace2 = whitespace_char | newline_char

let constituent = [
  '!'  '$'  '%'  '&'  '*'  '<'  '='  '>'
  '?'  '@'  '['  ']'  '^'  '_'  '~'  '{'
  '}'  '+'  '-'  '.'  '/'] | latin_char | numeric_char

let multi_escape_char = ['"'  '#'  '\'' '('  ')'  ','  ';'  '\\' '`'  ':']
  | invalid | whitespace2 | constituent

let token_str = constituent | '\\' lisp_std_char | '|' multi_escape_char* '|'  

(* Rules *)
rule read =
  parse
  | eof                         { EOF }
  | whitespace2                 { WHITESPACE }
  | invalid                     { raise (ReaderError "reader-error") }
  | '\\' eof                    { raise (EOFError "end-of-file") }
  | '|' multi_escape_char* eof        { raise (EOFError "end-of-file") }
  | '\''                        { raise (NotSupportedError "not-supported-error") }
  | '`'                         { raise (NotSupportedError "not-supported-error") }
  | ','                         { raise (NotSupportedError "not-supported-error") }
  | '('                         { OPEN_PAR }
  | ')'                         { CLOSE_PAR }
  | ';'                         { comment lexbuf }
  | "#|"                        { balanced_comment lexbuf }
  | '"'                         { make_string "" lexbuf }
  | '#'                         { sharp_sign lexbuf }
  | token_str+ as x             { TOKEN x }
  
and comment =
  parse
  | eof                         { EOF }
  | newline_char                { read lexbuf }
  | lisp_std_char               { comment lexbuf }
and balanced_comment =
  parse
  | eof                         { raise (EOFError "end-of-file") }
  | "|#"                        { read lexbuf }
  | lisp_std_char               { balanced_comment lexbuf }
and make_string acc =
  parse
  | eof                         { raise (EOFError "end-of-file") }
  | '"'                         { if "" != acc then STRING acc else read lexbuf }
  | "\\\\"                      { make_string (acc^"\\") lexbuf }
  | "\\\""                      { make_string (acc^"\"") lexbuf }
  | '\\'newline_char+           { make_string acc lexbuf }
  | '\n'                        { make_string (acc^"\n") lexbuf }
  | '\r'                        { make_string (acc^"\r") lexbuf }
  | '\012'                      { make_string (acc^"\012") lexbuf }
  | '\b'                        { make_string (acc^"\b") lexbuf }
  | '\127'                      { make_string (acc^"\127") lexbuf }
  | lisp_std_char as x          { make_string (acc^(Char.escaped x)) lexbuf }
and sharp_sign =
  parse
  | eof                         { raise (EOFError "end-of-file") }
  | '\\' (lisp_std_char as x)     { CHAR x }
  | ['b' 'B']                   { RADIX 2 }
  | ['o' 'O']                   { RADIX 8 }
  | ['x' 'X']                   { RADIX 16 }
  | (numeric_char+ as x) ['r' 'R'] { RADIX (int_of_string x) }
  | lisp_std_char               { raise (ReaderError "reader-error") }