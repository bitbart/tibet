.PHONY: clean


all:
	ocamlopt -c errors.ml
	ocamlopt -c tools.ml
	ocamlopt -c tipi.ml
	ocamlopt -c extTipi.ml
	ocamlopt -c mapping.ml
	ocamlopt -c -pp camlp4o python.ml
	ocamlopt -c kindsystem.ml
	ocamlopt -c toXML.ml
	ocamlopt -c fromXML.ml
	ocamlopt -c -pp camlp4o cparser.ml
	ocamlopt -c monitor.ml
	ocamlopt -c main.ml

	ocamlopt -o ctu unix.cmxa str.cmxa xml-light.cmxa errors.cmx tools.cmx tipi.cmx extTipi.cmx python.cmx kindsystem.cmx mapping.cmx toXML.cmx fromXML.cmx cparser.cmx monitor.cmx main.cmx


test:
	ocamlopt -c errors.ml
	ocamlopt -c tools.ml
	ocamlopt -c tipi.ml
	ocamlopt -c extTipi.ml
	ocamlopt -c mapping.ml
	ocamlopt -c -pp camlp4o python.ml
	ocamlopt -c kindsystem.ml
	ocamlopt -c toXML.ml
	ocamlopt -c fromXML.ml
	ocamlopt -c -pp camlp4o cparser.ml
	ocamlopt -c monitor.ml

	ocamlopt -c tests/testPython.ml
	ocamlopt -I tests -c tests/main.ml
	
	ocamlopt -o applicationTest unix.cmxa str.cmxa xml-light.cmxa errors.cmx tools.cmx tipi.cmx extTipi.cmx python.cmx kindsystem.cmx  mapping.cmx toXML.cmx fromXML.cmx cparser.cmx monitor.cmx tests/testPython.cmx tests/main.cmx


clean:
	@'rm' *.cmo *.cmi *.cmx *.o ctu 2>/dev/null || true
	@'rm' tests/*.cmi tests/*.cmx tests/*.o applicationTest 2>/dev/null || true 
