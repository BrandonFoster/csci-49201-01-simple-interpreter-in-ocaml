/* Common-Lisp Parser
 * Grammar Specification of Common-Lisp
 */
%{ open CommonLisp %}

%token EOF

%token <char> INVALID
%token <char> TERM_MACRO
%token <char> NON_TERM_MACRO
%token <char> ALPHABETIC2
%token <char * int list> CONSTITUENT
%token MULTI_ESCAPE
%token WHITESPACE

%start <CommonLisp.lisp_object option> program
%%

program:
  | EOF
    { None }
  | WHITESPACE* EOF
    { None }
  ;