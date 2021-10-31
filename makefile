# file "makefile"

all: delete built 
built: 
	ocamllex -q miniooLEX.mll
	ocamlc -c miniooAbstractSyntax.ml
#	menhir --infer --explain miniooMENHIR.mly
	menhir miniooMENHIR.mly
	ocamlc -c miniooMENHIR.mli
	ocamlc -c miniooLEX.ml
	ocamlc -c miniooMENHIR.ml
	ocamlc -c main.ml
	ocamlc  miniooAbstractSyntax.cmo miniooMENHIR.cmo miniooLEX.cmo main.cmo -o main

delete: 
	rm -f main *~ .*~ *.cmo *.cmi miniooLEX.ml miniooMENHIR.ml miniooMENHIR.mli miniooMENHIR.conflicts
