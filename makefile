
# scanner
tokenizer: scanner.ml
	ocamlc -o tokenizer scanner.ml
scanner.ml: scanner.mll
	ocamllex scanner.mll

.PHONY: clean all
clean:
	rm -f a.out tokenizer *.cm* scanner.ml
all: clean tokenizer
