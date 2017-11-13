test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

play:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte

zip:
	zip finalsrc.zip *.ml* *.mli

zipcheck:
	bash checkzip.sh

clean:
	ocamlbuild -clean
	rm -f finalsrc.zip

check:
	bash checkenv.sh && bash checktypes.sh
