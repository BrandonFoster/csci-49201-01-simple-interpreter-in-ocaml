#Compiler Settings
OC=ocamlopt str.cmxa
OL=ocamllex
OP=menhir --explain --infer

#Program Executable Name
EXE=ocamllisp

#Build Commands
all: $(EXE)

$(EXE): commonLisp.cmx parser.cmx lexer.cmx eval.cmx main.ml
	$(OC) -o $@ $^

%.cmx: %.ml
	$(OC) -c $^

parser.ml: parser.mly
	$(OP) parser.mly
	$(OC) -c parser.mli

lexer.ml: parser.mly lexer.mll 
	$(OL) lexer.mll

clean:
	rm -f $(EXE) *.cmi *.cmx *.o parser.mli parser.conflicts parser.ml lexer.ml
