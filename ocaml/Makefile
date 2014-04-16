.PHONY: clean

all:
	ocamlopt -c tipi.ml
	ocamlopt -c mapping.ml
	ocamlopt -c toXML.ml
	ocamlopt -c main.ml

	ocamlopt -o ctu str.cmxa xml-light.cmxa tipi.cmx mapping.cmx toXML.cmx main.cmx

clean:
	@'rm' *.cmo *.cmi *.cmx *.o ctu 2>/dev/null || true
