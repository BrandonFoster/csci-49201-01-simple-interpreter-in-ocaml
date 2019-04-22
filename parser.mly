/* Common-Lisp Parser
 * Grammar Specification of Common-Lisp */
%{
  open CommonLisp

  exception ParserError of string

  let rec make_atom str =
    let rec rd_token str i force_symbol acc =
      if i < String.length str then
        begin match String.get str i with
        | '\\' -> rd_escape str (i+1) true acc
        | '|' -> rd_multi_escape str (i+1) true acc
        | a -> rd_token str (i+1) force_symbol (acc^(Char.escaped (Char.uppercase_ascii a)))
        end
      else
        (force_symbol, acc)
    and rd_escape str i force_symbol acc =
      rd_token str (i+1) force_symbol (acc^(Char.escaped (String.get str i)))
    and rd_multi_escape str i force_symbol acc =
      match String.get str i with
      | '|' -> rd_token str (i+1) force_symbol acc
      | a -> rd_multi_escape str (i+1) force_symbol (acc^(Char.escaped a))
    in
    match rd_token str 0 false "" with
    | true, str2 -> Symbol str2
    | false, str2 ->
        begin match integer_check1 str2 None |> integer_check2 10 str2 |> ratio_check 10 str2 with
        | Some atom -> atom
        | None -> Symbol str2
        end
  and integer_check1 str prev_check =
    if None != prev_check then
      prev_check
    else if Str.string_match (Str.regexp "^\\([+-]?[0-9]+\\)[.]$") str 0 then
      Some (Number (Integer (int_of_string (Str.matched_group 1 str))))
    else
      None
  and integer_check2 base str prev_check =
    let rec check base str pos acc =
      let s_len = String.length str in
      if pos = s_len then
        Some (Number (Integer acc))
      else
        begin
          let code = Char.code (Char.uppercase_ascii (String.get str (s_len - pos - 1))) in
          if 48 <= code && code <= 57 && (code - 48) < base then
            let value = (code - 48) * (int_of_float ((float_of_int base) ** (float_of_int pos))) in
            check base str (pos + 1) (acc + value)
          else if 65 <= code && code <= 90 && (code - 55) < base then
            let value = (code - 55) * (int_of_float ((float_of_int base) ** (float_of_int pos))) in
            check base str (pos + 1) (acc + value)
          else if pos = (s_len - 1) && 43 = code then
            Some (Number (Integer acc))
          else if pos = (s_len - 1) && 45 = code then
            Some (Number (Integer (-acc)))
          else
            None
        end
    in
    if None != prev_check then
      prev_check
    else if Str.string_match (Str.regexp "^[+-]?[0-9a-zA-Z]+$") str 0 then
      check base str 0 0
    else
      None
  and ratio_check base str prev_check =
    if None != prev_check then
      prev_check
    else if Str.string_match (Str.regexp "^\\([+-]?[0-9a-zA-Z]+\\)/\\([0-9a-zA-Z]+\\)$") str 0 then
      begin
      let str1 = Str.matched_group 1 str in
      let str2 = Str.matched_group 2 str in
      match integer_check2 base str1 None, integer_check2 base str2 None with
      | Some (Number (Integer i1)), Some (Number (Integer i2)) ->
          if 0 != i2 then
            Some (Number (Ratio (i1, i2)))
          else
            raise (ParserError "parser-error")
      | _ -> None
      end
    else
      None
  let cons_list ls =
    List.fold_right (fun expr acc -> cons expr acc) ls (Leaf Nil)
%}

%token EOF
%token WHITESPACE
%token OPEN_PAR
%token CLOSE_PAR
%token <int> RADIX
%token <char> CHAR
%token <string> STRING
%token <string> TOKEN

%start <CommonLisp.expr> program
%%

program:
  | optional_exprs EOF
    { cons_list $1 }

optional_exprs:
  | optional_whitespaces
    { [] }
  | optional_whitespaces exprs
    { $2 }
exprs: rev_exprs { List.rev $1 }
rev_exprs:
  | expr optional_whitespaces
    { [$1] }
  | rev_exprs expr optional_whitespaces
    { $2::$1 }
expr:
  | atom
    { Leaf $1 }
  | OPEN_PAR optional_exprs CLOSE_PAR
    { cons_list $2 }

atom:
  | TOKEN
    { make_atom $1 }
  | RADIX TOKEN
    { 
      if $1 < 2 || $1 > 36 then
        raise (ParserError "parser-error")
      else
        begin match integer_check2 $1 $2 None |> ratio_check $1 $2 with
        | Some atom -> atom
        | None -> raise (ParserError "parser-error")
        end
    }
  | STRING
    { String $1 }
  | CHAR
    { Char $1 }

optional_whitespaces:
  | (* empty *)
    { }
  | whitespaces
    { }
whitespaces:
  | WHITESPACE
    { }
  | whitespaces WHITESPACE
    { }
