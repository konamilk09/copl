SOURCES = value.ml syntax.ml parser.mly lexer.mll \
		main.ml
RESULT = ml3.o
OCAMLMAKEFILE = $(OPAM_SWITCH_PREFIX)/lib/ocaml-makefile/OCamlMakefile
include $(OCAMLMAKEFILE)