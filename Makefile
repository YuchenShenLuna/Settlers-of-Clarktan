test:
	ocamlbuild -use-ocamlfind state_test.byte && ./state_test.byte

play:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte

zip:
	zip finalsrc.zip *.ml* *.mli

zipcheck:
	bash checkzip.sh

clean:
	ocamlbuild -clean
	rm -f finalsrc.zip
