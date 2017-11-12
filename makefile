
newbie.native:
	ocamlbuild -use-ocamlfind -pkgs llvm, llvm.analysis -cflags -w,+a-4 newbie.native

OBJS = scanner.cmx

newbie: $(OBJS)
	ocamlfind ocamlopt -linkpkg -package llvm -package llvm.analysis $(OBJS) -o newbie

scanner.ml: scanner.mll
	ocamllex scanner.mll

%.cmo: %.ml
	ocamlc -c $<
%.cmi: %.mli
	ocamlc -c $<
%.cmx: %.ml
	ocamlfind ocamlopt -c -package llvm $<

newbie.cmo: scanner.cmo 
newbie.cmx: scanner.cmx


.PHONY: clean
clean:
	ocamlbuild -clean
	rm -rf *.diff newbie scanner.ml parser.ml parser.mli
	rm -rf *.cmx *.cmi *.cmo *.cmx *.o *.s *.ll *.out *.exe

all: newbie.native
