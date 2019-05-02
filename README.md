# OCamlLisp - Common Lisp Interpreter
## Prerequisites
	# Install Menhir
	opam install menhir

## Build
	# Build from source
	make

## Run
	# Lisp-code file as first and only argument
	./ocamllisp "program.lisp"

## Supported Features
  - PRINT function, printing data types.
  - SETQ function, setting global variables.
  - DOTIMES macro, looping.
  - Integer Arithmetic: Addition/Subtraction/Multiplication/Division.
  - Data Types: Symbol/Integer/Ratio/String/Char.
  - Radix Conversions e.g. #B1001 to Integer 9, #X12/21 to Ratio 6/11