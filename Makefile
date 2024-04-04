SOURCES = syntax.ml parser.mly lexer.mll eval.ml tree.ml main.ml
RESULT = ml1err.o
OCAMLMAKEFILE = $(OPAM_SWITCH_PREFIX)/lib/ocaml-makefile/OCamlMakefile
include $(OCAMLMAKEFILE)