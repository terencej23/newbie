OBJS = ast.cmx codegen.cmx parser.cmx scanner.cmx newbie.cmx

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
codegen.cmo: ast.cmo
codegen.cmx: ast.cmx
newbie.cmo: scanner.cmo parser.cmi codegen.cmo ast.cmo
newbie.cmx: scanner.cmx parser.cmx codegen.cmx ast.cmx 
parser.cmo: ast.cmo parser.cmi
parser.cmx: ast.cmx parser.cmi
scanner.cmo: parser.cmi
scanner.cmx: parser.cmx
parser.cmi: ast.cmo


.PHONY: clean
clean:
	ocamlbuild -clean
	rm -rf *.diff newbie scanner.ml parser.ml parser.mli
	rm -rf *.cmx *.cmi *.cmo *.cmx *.o *.s *.ll *.out *.exe

all: newbie
