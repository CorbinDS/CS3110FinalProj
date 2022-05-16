.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop lib

test:
	OCAMLRUNPARAM=b dune exec test/final_project.exe

run:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:
	rm -f final_project.zip
	zip -r final_project.zip . -x@exclude.lst

clean:
	dune clean
	ocamlbuild -clean
	rm -f final_project.zip

lines: 
	ocamlbuild -clean
	cloc --by-file --include-lang=OCaml .
	
doc:
	dune build @doc
