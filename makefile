OBJS = exceptions.cmx ast.cmx sast.cmx codegen.cmx parser.cmx scanner.cmx semant.cmx newbie.cmx

newbie: $(OBJS)
	ocamlfind ocamlopt -linkpkg -package llvm -package llvm.analysis $(OBJS) -o newbie

scanner.ml: scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli: parser.mly
	ocamlyacc parser.mly


%.cmo: %.ml
	ocamlc -c $<
%.cmi: %.mli
	ocamlc -c $<
%.cmx: %.ml
	ocamlfind ocamlopt -c -package llvm $<


ast.cmo:
ast.cmx:
exceptions.cmo:
exceptions.cmx:
sast.cmo: ast.cmo
sast.cmx: ast.cmx
codegen.cmo: ast.cmo
codegen.cmx: ast.cmx
newbie.cmo: semant.cmo scanner.cmo parser.cmi codegen.cmo sast.cmo ast.cmo exceptions.cmo
newbie.cmx: semant.cmx scanner.cmx parser.cmx codegen.cmx sast.cmo ast.cmx exceptions.cmx
parser.cmo: ast.cmo parser.cmi
parser.cmx: ast.cmx parser.cmi
scanner.cmo: parser.cmi
scanner.cmx: parser.cmx
semant.cmo : sast.cmo ast.cmo
semant.cmx : sast.cmx ast.cmx
parser.cmi: ast.cmo


.PHONY: clean
clean:
	ocamlbuild -clean
	rm -rf *.diff newbie scanner.ml parser.ml parser.mli
	rm -rf *.cmx *.cmi *.cmo *.cmx *.o *.s *.ll *.out *.exe

all: newbie
