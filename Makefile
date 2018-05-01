all: cex3

cex3:: # rule with no dependencies, is always triggered
	ocamlbuild -lib unix -lib str -I cb/main -I cb/libs cex3.native
	mv _build/cex3.native ./cex3
	unlink cex3.native

clean: 
	ocamlbuild -clean;
