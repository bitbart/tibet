.PHONY: clean

all:
	ocamlopt -c errors.ml
	ocamlopt -c tipi.ml
	ocamlopt -c mapping.ml
	ocamlopt -c toXML.ml
	ocamlopt -c fromXML.ml
	ocamlopt -c -pp camlp4o cparser.ml
	ocamlopt -c monitor.ml
	ocamlopt -c main.ml

	ocamlopt -o ctu str.cmxa xml-light.cmxa errors.cmx tipi.cmx mapping.cmx toXML.cmx fromXML.cmx cparser.cmx monitor.cmx main.cmx

clean:
	@'rm' *.cmo *.cmi *.cmx *.o ctu 2>/dev/null || true
