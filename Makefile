SOURCES = value.ml env.ml syntax.ml parser.mly lexer.mll \
		eval.ml tree.ml main.ml
RESULT = ml2.o
OCAMLMAKEFILE = $(OPAM_SWITCH_PREFIX)/lib/ocaml-makefile/OCamlMakefile
include $(OCAMLMAKEFILE)