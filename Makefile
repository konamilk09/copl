SOURCES = syntax.ml value.ml parser.mly lexer.mll \
		eval.ml main.ml
RESULT = ml3.o
OCAMLMAKEFILE = $(OPAM_SWITCH_PREFIX)/lib/ocaml-makefile/OCamlMakefile
include $(OCAMLMAKEFILE)