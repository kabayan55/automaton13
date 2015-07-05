SOURCES = syntax.ml parser.mly lexer.mll world_syntax.ml world_parser.mly world_lexer.mll main.ml
RESULT = game
OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)

.SUFFIXES: .mll .mllz
.mllz.mll :
	/home/isstaff/asai/bin/ocamllex-zenkaku $< > $*.mll
