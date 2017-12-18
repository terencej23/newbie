OBJS = exceptions.cmx ast.cmx sast.cmx codegen.cmx parser.cmx scanner.cmx semant.cmx newbie.cmx

.PHONY: all
all: newbie print.o

newbie: $(OBJS)
	ocamlfind ocamlopt -linkpkg -fPIC -package llvm -package llvm.analysis $(OBJS) -o newbie

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


newbie.cmo: semant.cmo scanner.cmo parser.cmi codegen.cmo sast.cmo ast.cmo exceptions.cmo 
newbie.cmx: semant.cmx scanner.cmx parser.cmx codegen.cmx sast.cmo ast.cmx exceptions.cmx
scanner.cmo: parser.cmi exceptions.cmo
scanner.cmx: parser.cmx exceptions.cmx
parser.cmo: ast.cmo parser.cmi
parser.cmx: ast.cmx parser.cmi
parser.cmi: ast.cmo
ast.cmo:
ast.cmx:
sast.cmo: ast.cmo
sast.cmx: ast.cmx
semant.cmo: sast.cmo ast.cmo exceptions.cmo
semant.cmx: sast.cmx ast.cmx exceptions.cmx
codegen.cmo: semant.cmo sast.cmo ast.cmo
codegen.cmx: semant.cmx sast.cmo ast.cmx
exceptions.cmo:
exceptions.cmx:

node : node.c
	cc -o strcmp -DBUILD_TEST node.c

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -rf *.diff newbie scanner.ml parser.ml parser.mli
	rm -rf *.cmx *.cmi *.cmo *.cmx *.o *.s *.ll *.out *.exe
	rm -rf print

print: print.c
	gcc -o print print.c
